module AHD.Labs.Lab1 where

import Clash.Prelude

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    s = a `xor` b `xor` cIn
    cOut = (cIn .&. (a `xor` b)) .|. (a .&. b)

-- | A 3-bit adder
--
-- The adder computes the following sum \(a + b = (a_2 a_1 a_0)_2 + (b_2 b_1 b_0)_2 = (c, s_2, s_1, s_0)\).
threeBitAdder ::
  Bit ->
  Bit ->
  Bit ->
  Bit ->
  Bit ->
  Bit ->
  (Bit, Bit, Bit, Bit)
threeBitAdder a2 a1 a0 b2 b1 b0 = (c, s2, s1, s0)
  where
    (cOut1, s0) = fullAdder a0 b0 0
    (cOut2, s1) = fullAdder a1 b1 cOut1
    (c, s2) = fullAdder a2 b2 cOut2
