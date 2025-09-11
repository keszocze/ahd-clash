{-# OPTIONS_GHC -Wno-unused-imports #-}
module AHD.Labs.Solutions.Lab3 where


-- intentionally imported so that one can use the helper function from clashi
import AHD.Util
import Clash.Class.Counter
import Clash.Prelude


-- | Delays inputs by two clock cycles
--
-- > clashi> simulateN 10 delay2 [1,2,3,4,5,6,7,8,9]
-- > [42,42,1,2,3,4,5,6,7,8]
delay2 ::
  -- Constraint to tell `register` that it has clock/reset/enable signals
  (HiddenClockResetEnable System) =>
  -- | The signal to delay
  Signal System (Unsigned 8) ->
  -- | The delayed signal
  Signal System (Unsigned 8)
delay2 v = v''
  where
    v' = register 42 v
    v'' = register 42 v'

-- | Counter counting up to a given value @k@ and then wrapping around
--
-- > clashi> sampleN 10 (counter @3 2)
-- > [0,0,1,2,0,1,2,0,1,2]
counter ::
  ( HiddenClockResetEnable System,
    KnownNat n
  ) =>
  -- | The upper bound @k@
  Unsigned n ->
  Signal System (Unsigned n)
counter k = r
  where
    r = register 0 (fmap updFun r)
    updFun v = if v == k then 0 else v + 1

-- | A clock counting in two different bit-widths
--
-- > clashi> sampleN 10 (clock @3 @2)
-- > [(0,0),(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0)]
clock ::
  (HiddenClockResetEnable System, KnownNat n, KnownNat m) =>
  Signal System (Unsigned n, Unsigned m)
clock = r
  where
    r = register (0, 0) (fmap countSucc r)

-- | Removes even number by replacing them with @0@
--
-- > clashi> simulateN 11 zeroIfEven [0 :: Unsigned 4..10]
-- > [0,1,0,3,0,5,0,7,0,9,0]
zeroIfEven ::
  (KnownNat n) =>
  Signal System (Unsigned n) ->
  Signal System (Unsigned n)
zeroIfEven = fmap (\v -> if even v then 0 else v)

-- | A circuit that takes values \(v\) and computes \(2\cdot v + 3\)
--
-- > clashi> simulateN 11 timesTwoPlusThree  [0 :: Unsigned 8..10]
-- > [3,5,7,9,11,13,15,17,19,21,23]
timesTwoPlusThree ::
  (KnownNat n) =>
  -- | The input stream of values to compute with
  Signal System (Unsigned n) ->
  -- | The input value times two and then plus three
  Signal System (Unsigned n)
timesTwoPlusThree s = 2 * s + 3

-- $setup
-- >>> import Clash.Prelude
