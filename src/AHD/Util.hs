

-- | Utilitiy functions that help evaluating hardware in the REPL/Clashi
module AHD.Util (
  -- * Bit-level function helpers
  eval2, truthTable2, truthTable2', eval3, truthTable3, truthTable3',
  -- * Mealy machine helpers
  debugMealy, addDebugInfo,
  -- * Helpers for simulating sequential hardware
  prettySampleN, prettySimulateN, tuple2, tuple3, bitInputs2, bitInputs3
  ) where

import Clash.Prelude
import Clash.Class.Counter

-- | Creates the truth table for a 2-bit `Bit`-function
--
-- >>> truthTable2 (\a b -> a `xor` b)
-- [0,1,1,0]
truthTable2 :: (Bit -> Bit -> a) -> [a]
truthTable2 f = [f a b | a <- [0, 1], b <- [0, 1]]

-- | Creates the truth table for a 3-bit `Bit`-function
--
-- >>> truthTable3 (\a b c -> (a `xor` b) .|. c)
-- [0,1,1,1,1,1,0,1]
truthTable3 :: (Bit -> Bit -> Bit -> a) -> [a]
truthTable3 f = [f a b c | a <- [0, 1], b <- [0, 1], c <- [0, 1]]

-- | Creates the truth table for a 2-bit `Bit`-function prepending the inputs to the function's value
--
-- >>> truthTable2' (\a b -> a `xor` b)
-- [(0,0,0),(0,1,1),(1,0,1),(1,1,0)]
truthTable2' :: (Bit -> Bit -> a) -> [(Bit, Bit, a)]
truthTable2' f = [(a, b, f a b) | a <- [0, 1], b <- [0, 1]]

-- | Creates the truth table for a 3-bit `Bit`-function prepending the inputs to the function's value
--
--  >>> truthTable3' (\a b c -> (a `xor` b) .|. c)
--  [(0,0,0,0),(0,0,1,1),(0,1,0,1),(0,1,1,1),(1,0,0,1),(1,0,1,1),(1,1,0,0),(1,1,1,1)]
truthTable3' :: (Bit -> Bit -> Bit -> a) -> [(Bit, Bit, Bit, a)]
truthTable3' f = [(a, b, c, f a b c) | a <- [0, 1], b <- [0, 1], c <- [0, 1]]

-- | Pretty prints the truth table of a 2-bit `Bit`-function
--
-- > clashi> eval2 (\a b -> a `xor` b)
-- > a b | f(a, b)
-- > -------------
-- > 0 0 | 0
-- > 0 1 | 1
-- > 1 0 | 1
-- > 1 1 | 0
eval2 :: (Show a) => (Bit -> Bit -> a) -> IO ()
eval2 f = do
  putStrLn "a b | f(a, b)"
  putStrLn "-------------"
  mapM_ (\(a, b, v) -> putStrLn $ show a <> " " <> show b <> " | " <> show v) vals
  where
    vals = truthTable2' f

-- | Pretty prints the truth table of a 3-bit `Bit`-function
--
-- > clashi> eval3 (\a b c -> (a `xor` b) .|. c)
-- > a b c | f(a, b, c)
-- > ------------------
-- > 0 0 0 | 0
-- > 0 0 1 | 1
-- > 0 1 0 | 1
-- > 0 1 1 | 1
-- > 1 0 0 | 1
-- > 1 0 1 | 1
-- > 1 1 0 | 0
eval3 :: (Show a) => (Bit -> Bit -> Bit -> a) -> IO ()
eval3 f = do
  putStrLn "a b c | f(a, b, c)"
  putStrLn "------------------"
  mapM_ (\(a, b, c, v) -> putStrLn $ show a <> " " <> show b <> " " <> show c <> " | " <> show v) vals
  where
    vals = truthTable3' f

-- | Augments a combined transition/output function with debug information
--
-- Original output is replace by a tuple consisting of
--
-- * the state before the current input was processed @s@
-- * the current input @i@
-- * the next state computed from @s@ and @i@
-- * the next output computed from @s@ and @i@
addDebugInfo ::
  -- | The combined transition/output function
  (s -> i -> (s, o)) ->
  -- | The augmented transition/output function
  (s -> i -> (s, (s, i, s, o)))
addDebugInfo f s i = let (s', o) = f s i in (s', (s, i, s', o))

-- | Augmentation of the `mealy` function with debug information
--
-- Use  this function in conjunction with `prettySimulateN` to debug your Mealy machines.
debugMealy ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  -- | The combined transition/output function
  (s -> i -> (s, o)) ->
  -- | The initial state
  s ->
  -- | The input stream
  Signal dom i ->
  -- | The output stream
  Signal dom (s, i, s, o)
debugMealy f = mealy (addDebugInfo f)



-- | Pretty prints a simulation run
--
-- > clashi> prettySimulateN @System 6 (fmap (\n -> 2*n)) [0..5]
-- > 0
-- > 2
-- > 4
-- > 6
-- > 8
-- > 10

prettySimulateN ::
  (KnownDomain dom, NFDataX a, NFDataX b, Show b) =>
  Int ->
  ((HiddenClockResetEnable dom) => Signal dom a -> Signal dom b) ->
  [a] ->
  IO ()
prettySimulateN n f vals = mapM_ print $ simulateN n f vals

-- | Pretty prints a sample run
--
-- > clashi> prettySampleN 10 (clock @2 @2)
-- > (0,0)
-- > (0,0)
-- > (0,1)
-- > (0,2)
-- > (0,3)
-- > (1,0)
-- > (1,1)
-- > (1,2)
-- > (1,3)
-- > (2,0)
prettySampleN ::
  (KnownDomain dom, NFDataX a, Show a) =>
  Int ->
  ((HiddenClockResetEnable dom) => Signal dom a) ->
  IO ()
prettySampleN n f = mapM_ print $ sampleN n f


-- | Converts a function expecting two inputs into one expecting one two-tuple instead.
tuple2 :: (Signal System a -> Signal System b -> Signal System c) -> Signal System (a, b) -> Signal System c
tuple2 f vals = (uncurry f) (unbundle vals)

-- | Converts a function expecting three inputs into one expecting one three-tuple instead.
tuple3 :: (Signal System a -> Signal System b -> Signal System c -> Signal System d) -> Signal System (a, b, c) -> Signal System d
tuple3 f vals = f a b c
  where
    (a, b, c) = unbundle vals


bitInputs2 :: SystemClockResetEnable => Signal System (Bit,Bit)
bitInputs2 = fmap bitCoerce r
  where r = register (0 :: Unsigned 1, 0 :: Unsigned 1) (fmap countSucc r)

bitInputs3 :: SystemClockResetEnable => Signal System (Bit,Bit, Bit)
bitInputs3 = fmap bitCoerce r
  where r = register (0 :: Unsigned 1, 0 :: Unsigned 1, 0 :: Unsigned 1) (fmap countSucc r)

-- $setup
-- >>> import Clash.Prelude
