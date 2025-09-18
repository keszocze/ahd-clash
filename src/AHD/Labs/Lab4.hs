module AHD.Labs.Lab4 where

import AHD.Util
import Clash.Prelude


-- | Combined transition/output function for a Mealy machine that outputs how often the value @3@ was input modulo @5@
--
-- It is used by `threeCounter` to create the sequential circuit that does the counting.
threeCounter' :: Unsigned 8 -> Unsigned 2 -> (Unsigned 8, Unsigned 3)
threeCounter' cnt input = undefined

-- | Sequential circuit counting how often the value @3@ was input modulo @5@
threeCounter :: SystemClockResetEnable => Signal System (Unsigned 2) -> Signal System (Unsigned 3)
threeCounter = mealy threeCounter' 0


-- | Sequential circuit counting how often the value @3@ was input modulo @5@ providing additional debug output
threeCounterDebug :: SystemClockResetEnable =>
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
counter k s advance = undefined

-- | Optional
lowerClock :: (SystemClockResetEnable, KnownNat m) => Unsigned m -> Signal System Bit -> Signal System (Bit, Unsigned m)
lowerClock k = undefined

-- | Optional
upperClock :: (SystemClockResetEnable, KnownNat n) => Unsigned n -> Signal System Bit -> Signal System (Bit, Unsigned n)
upperClock k = undefined

-- | Optional
clock :: (SystemClockResetEnable, KnownNat n, KnownNat m) => Unsigned n -> Unsigned m -> Signal System Bit -> Signal System (Unsigned n, Unsigned m)
clock kUpper kLower i = undefined
