module AHD.Labs.Solutions.Solution1 where

import Clash.Prelude


fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    s = a `xor` b `xor` cIn
    cOut = (cIn .&. (a `xor` b)) .|. (a .&. b)


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
