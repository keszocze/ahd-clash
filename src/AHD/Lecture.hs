{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module collecting examples and code from the AHD Clash lecture.
module AHD.Lecture where

-- intentionall imported to be used when interactively showing things
-- during the lecture
import AHD.Util
import Clash.Prelude

-- | A half adder circuit
--
-- The half adder computes the equations
--
-- \[
-- \begin{aligned}
--    c_{out} &= a \wedge b \\
--    s &= a \oplus b
-- \end{aligned}
-- \]
halfAdder ::
  -- | The bit \(a\)
  Bit ->
  -- | The bit \(b\)
  Bit ->
  -- | The carry and sum \((a \wedge b, a \oplus b)\)
  (Bit, Bit)
halfAdder a b = (cOut, s)
  where
    cOut = a .&. b
    s = a `xor` b

-- | The function that will be synthesized to Verilog/VHDL
--
-- Let this function point to what you implemented.
topEntity = halfAdder

-- | A simple multiplexer
--
-- >>> myMux False 0b110 0b011
-- 6
-- >>> myMux True 0b110 0b011
-- 3
myMux :: Bool -> p -> p -> p
myMux False a _ = a
myMux True _ b = b

-- | A four-way multiplexer with a *direct* implementation
fourWayMuxDirect :: Unsigned 2 -> a -> a -> a -> a -> a
fourWayMuxDirect 0 a _ _ _ = a
fourWayMuxDirect 1 _ b _ _ = b
fourWayMuxDirect 2 _ _ c _ = c
fourWayMuxDirect 3 _ _ _ d = d

-- | A four-way multiplexer using a `Vec` and indexing for the implementation
fourWayMuxVec :: Unsigned 2 -> Vec 4 a -> a
fourWayMuxVec idx values = values !! idx

{-# ANN
  namedTopEntity
  ( Synthesize
      { t_name = "half_hadder",
        t_inputs =
          [ PortName "a",
            PortName "b"
          ],
        t_output =
          PortProduct
            "TheSum"
            [ PortName "c_out",
              PortName "s"
            ]
      }
  )
  #-}

-- | A top entity for the half adder that clearly specifies how to name the inputs and outputs
--
-- Synthesize this via
--
-- > stack run clash -- src/AHD/Lecture.hs --verilog -main-is namedTopEntity
namedTopEntity :: Bit -> Bit -> (Bit, Bit)
namedTopEntity a b = halfAdder a b

-- | A simple 4-bit counter
--
-- >>> sampleN 20 cnt
-- [0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2]
cnt :: (SystemClockResetEnable) => Signal System (Unsigned 4)
cnt = theCount
  where
    theCount = register 0 (theCount + 1)

-- | A 3-bit counter with variable step size
--
-- >>> sampleN 10 (fmapCntStep 2)
-- [0,0,2,4,6,0,2,4,6,0]
-- >>> sampleN 10 (fmapCntStep 3)
-- [0,0,3,6,1,4,7,2,5,0]
fmapCntStep :: (SystemClockResetEnable) => Unsigned 3 -> Signal System (Unsigned 3)
fmapCntStep step = theCount
  where
    theCount = register 0 (fmap (\c -> c + step) theCount)

-- | A counter using a one-hot encoding
--
-- >>> sampleN 10 oneHotCounter
-- [0b0001,0b0001,0b0010,0b0100,0b1000,0b0001,0b0010,0b0100,0b1000,0b0001]
oneHotCounter :: (SystemClockResetEnable) => Signal System (BitVector 4)
oneHotCounter = theCount
  where
    theCount = register 0b0001 (fmap (\c -> rotateL c 1) theCount)

-- | A simple sequential circuit doubling its input
--
-- >>> simulateN 5 seqDoubler [1,2,3,4,10]
-- [2,4,6,8,20]
seqDoubler :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
seqDoubler a = 2 * a

-- | A half adder operating on `Signal`s
--
-- You may want to use the `tuple2` function when simulating.
--
-- >>> simulateN 4 (tuple2 seqHalfAdder) [(0,0 ), (0,1), (1,0), (1,1)]
-- [(0,0),(0,1),(0,1),(1,0)]
seqHalfAdder :: (SystemClockResetEnable) => Signal System Bit -> Signal System Bit -> Signal System (Bit, Bit)
seqHalfAdder a b = halfAdder <$> a <*> b

-- | A simple example for how to use a register
--
-- >>> sampleN 8 myReg
-- [3,3,4,4,4,4,4,4]
myReg :: (SystemClockResetEnable) => Signal System (Unsigned 4)
myReg = register 3 (pure 4)

-- | Function computing the parity based on the current parity and the next incoming bit
--
-- This method is a combined transition and output function to be used with `mealy` to create the
-- `parityMealy` function.
parity ::
  -- | Current parity
  Bool ->
  -- | Next incoming bit
  Bit ->
  -- | Tuple of current parity state and the output of the state
  (Bool, Bool)
parity p i = (p', p')
  where
    p' = if i == 1 then not p else p

-- | Circuit computing the parity of the input seen so far
--
-- >>> simulateN 6 parityMealy [0,1,1,0,1,1]
-- [False,True,False,False,True,False]
parityMealy ::
  (SystemClockResetEnable) =>
  -- | Stream of inputs whose parity is being computed
  Signal System Bit ->
  Signal System Bool
parityMealy = mealy parity False
