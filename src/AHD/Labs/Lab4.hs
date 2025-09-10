module AHD.Labs.Lab4 where

import AHD.Util
import Clash.Prelude



threeCounter' :: Unsigned 8 -> Unsigned 2 -> (Unsigned 8, Unsigned 3)
threeCounter' cnt input = (cnt', resize $ cnt' `mod` 5)
      where cnt' = if input == 3 then cnt + 1 else cnt


threeCounter :: (HiddenClockResetEnable System) => Signal System (Unsigned 2) -> Signal System (Unsigned 3)
threeCounter = mealy threeCounter' 0


threeCounterDebug :: (HiddenClockResetEnable System) => Signal System (Unsigned 2) -> Signal System (Unsigned 8, Unsigned 2, Unsigned 8, Unsigned 3)
threeCounterDebug = debugMealy threeCounter' 0




counter :: (KnownNat n) => Unsigned n -> Unsigned n -> Bit -> (Unsigned n, (Bit, Unsigned n))
counter k s advance =
  if advance == 1
    then (s', (advanceNext, s'))
    else (s, (0, s))
  where
      (advanceNext, s')  = if s == k then (1,0) else (0,s+1)

lowerClock :: (HiddenClockResetEnable System, KnownNat m) => Unsigned m -> Signal System Bit -> Signal System (Bit, Unsigned m)
lowerClock k = mealy @System (counter k) 0


upperClock :: (HiddenClockResetEnable System, KnownNat n) => Unsigned n -> Signal System Bit -> Signal System (Bit, Unsigned n)
upperClock k = mealy @System (counter k) 0


clock :: (HiddenClockResetEnable System, KnownNat n, KnownNat m) => Unsigned n -> Unsigned m -> Signal System Bit -> Signal System (Unsigned n, Unsigned m)
clock kUpper kLower i = bundle (lClock, rClock)
      where
            (wrap,rClock) = unbundle $ (lowerClock kLower) i
            (_, lClock) = unbundle $ (upperClock kUpper) wrap
