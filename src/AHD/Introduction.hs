
-- | Module collecting examples and code from the AHD Clash lecture.
module AHD.Introduction where

import AHD.Util

import Clash.Prelude

-- | A half adder circuit
--
-- The half adder computes the equations
--
-- $$
-- \begin{aligned}
--    c_{out} &= a \wedge b \\
--    s &= a \oplus b
-- \end{aligned}
-- $$
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
