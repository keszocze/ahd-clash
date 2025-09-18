{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Module to implement the solutions to the first lab in
module AHD.Labs.Lab1 where

-- intentionally imported so that one can use the helper function from clashi
import AHD.Util
import Clash.Prelude

-- | A full adder circuit
--
-- A full adder implements the following equations:
--
-- \[
-- \begin{aligned}
-- 	c_{out} &= (c_{in} \wedge (a\oplus b)) \vee (a \wedge b)\\
-- 	s &= a \oplus b \oplus c_{in}.
-- \end{aligned}
-- \]
--
-- ===Example
--
-- > clashi> fullAdder 0 1 1
-- > (1,0)
-- > clashi> fullAdder 1 1 1
-- > (1,1)
fullAdder ::
  -- | The bit \(a\)
  Bit ->
  -- | The bit \(b\)
  Bit ->
  -- | The bit \(c_{in}\)
  Bit ->
  -- | The carry and sum \((c_{out}, s)\)
  (Bit, Bit)
fullAdder a b cIn = undefined

-- | A 3-bit adder
--
-- The adder computes the following sum \(a + b = (a_2 a_1 a_0)_2 + (b_2 b_1 b_0)_2 = (c, s_2, s_1, s_0)_2\).
--
-- The order of parameters is chosen so that calling the functions resembles writing the number as a binary number.
--
-- ===Example
--
-- > clashi> threeBitAdder 1 0 0 0 0 1
-- > (0,1,0,1)
-- >
-- > clashi> threeBitAdder 1 1 1 0 0 1
-- > (1,0,0,0)
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
  -- |  The sum \((c, s_2, s_1, s_0)\)
  (Bit, Bit, Bit, Bit)
threeBitAdder a2 a1 a0 b2 b1 b0 = undefined
