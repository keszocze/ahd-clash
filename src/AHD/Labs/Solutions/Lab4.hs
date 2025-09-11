module AHD.Labs.Lab4 where

import AHD.Util
import Clash.Prelude


-- | Combined transition/output function for a Mealy machine that outputs how often the value @3@ was input modulo @5@
--
-- It is used by `threeCounter` to create the sequential circuit that does the counting.
threeCounter' :: Unsigned 8 -> Unsigned 2 -> (Unsigned 8, Unsigned 3)
threeCounter' cnt input = (cnt', resize $ cnt' `mod` 5)
      where cnt' = if input == 3 then cnt + 1 else cnt

-- | Sequential circuit counting how often the value @3@ was input modulo @5@
threeCounter :: (HiddenClockResetEnable System) => Signal System (Unsigned 2) -> Signal System (Unsigned 3)
threeCounter = mealy threeCounter' 0


-- | Sequential circuit counting how often the value @3@ was input modulo @5@ providing additional debug output
threeCounterDebug :: (HiddenClockResetEnable System) =>
      Signal System (Unsigned 2) ->
      -- | Tuple consisting of
      --
      -- * The previous state (i.e., the number of @3@'s seen so far)
      -- * The current input
      -- * The next state
      -- * The next output (i.e., the number of @3@'s modulo @5@)
      Signal System (Unsigned 8, Unsigned 2, Unsigned 8, Unsigned 3)
threeCounterDebug = debugMealy threeCounter' 0



-- | Optional
counter :: (KnownNat n) => Unsigned n -> Unsigned n -> Bit -> (Unsigned n, (Bit, Unsigned n))
counter k s advance =
  if advance == 1
    then (s', (advanceNext, s'))
    else (s, (0, s))
  where
      (advanceNext, s')  = if s == k then (1,0) else (0,s+1)

-- | Optional
lowerClock :: (HiddenClockResetEnable System, KnownNat m) => Unsigned m -> Signal System Bit -> Signal System (Bit, Unsigned m)
lowerClock k = mealy @System (counter k) 0

-- | Optional
upperClock :: (HiddenClockResetEnable System, KnownNat n) => Unsigned n -> Signal System Bit -> Signal System (Bit, Unsigned n)
upperClock k = mealy @System (counter k) 0

-- | Optional
clock :: (HiddenClockResetEnable System, KnownNat n, KnownNat m) => Unsigned n -> Unsigned m -> Signal System Bit -> Signal System (Unsigned n, Unsigned m)
clock kUpper kLower i = bundle (lClock, rClock)
      where
            (wrap,rClock) = unbundle $ (lowerClock kLower) i
            (_, lClock) = unbundle $ (upperClock kUpper) wrap
