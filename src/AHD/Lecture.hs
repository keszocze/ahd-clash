-- | Module collecting examples and code from the AHD Clash lecture.
module AHD.Lecture where

import AHD.Util
import Clash.Prelude


-- | A half adder circuit
--
-- The half adder computes the equations

-- $$
-- \begin{aligned}
--    c_{out} &= a \wedge b \\
--    s &= a \oplus b
-- \end{aligned}

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

-- | A four-way multiplexer with a _direct_ implementation
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

cnt :: SystemClockResetEnable => Signal System (Unsigned 4)
cnt = theCount
  where
    theCount = register 0 (theCount + 1)

fmapCntStep :: SystemClockResetEnable => Unsigned 3 -> Signal System (Unsigned 3)
fmapCntStep step = theCount
  where
    theCount = register 0 (fmap (\c -> c + step) theCount)

oneHotCounter :: SystemClockResetEnable => Signal System (BitVector 4)
oneHotCounter = theCount
  where
    theCount = register 0b0001 (fmap (\c -> rotateL c 1) theCount)

seqDoubler :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
seqDoubler a = 2 * a

seqHalfAdder :: SystemClockResetEnable => Signal System Bit -> Signal System Bit -> Signal System (Bit, Bit)
seqHalfAdder a b = halfAdder <$> a <*> b


myReg :: SystemClockResetEnable => Signal System (Unsigned 4)
myReg = register 3 (pure 4)


parity :: Bool -> Bit -> (Bool, Bool)
parity p i = (p',p')
  where p' = if i == 1 then not p else p

parityMealy :: SystemClockResetEnable =>
  Signal System Bit -> Signal System Bool
parityMealy = mealy parity False





