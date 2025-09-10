module AHD.Labs.Lab1 where

import Clash.Prelude

-- intentionally imported so that one can use the helper function from clashi
import AHD.Util


-- | A full adder circuit
--
-- A full adder implements the following equations:
-- $$
-- \begin{aligned}
-- 	c_{out} &= (c_{in} \wedge (a\oplus b)) \vee (a \wedge b)\\
-- 	s &= a \oplus b \oplus c_{in}.
-- \end{aligned}
-- $
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    s = a `xor` b `xor` cIn
    cOut = (cIn .&. (a `xor` b)) .|. (a .&. b)



-- | A 3-bit adder
--
-- The adder computes the following sum \(a + b = (a_2 a_1 a_0)_2 + (b_2 b_1 b_0)_2 = (c, s_2, s_1, s_0)_2\).
--
-- The order of parameters is chosen so that calling the functions resembles writing
threeBitAdder ::
  -- |  The bit \(a_2\)
  Bit ->
    -- |  The bit \(a_1\)
  Bit ->
    -- |  The bit \(a_0\)
  Bit ->
    -- |  The bit \(b_2\)
  Bit ->
    -- |  The bit \(b_1\)
  Bit ->
    -- |  The bit \(b_0\)
  Bit ->
    -- |  The sum \(c, s_2, s_1, s_0)\)
  (Bit, Bit, Bit, Bit)
threeBitAdder a2 a1 a0 b2 b1 b0 = (c, s2, s1, s0)
  where
    (cOut1, s0) = fullAdder a0 b0 0
    (cOut2, s1) = fullAdder a1 b1 cOut1
    (c, s2) = fullAdder a2 b2 cOut2
