module AHD.Labs.Lab2 where

import Clash.Prelude

import AHD.Util

-- | A 3-bit adder built using `Vec`
--
-- The adder computes the following sum \(a + b = (a_2 a_1 a_0)_2 + (b_2 b_1 b_0)_2 = (c, s_2, s_1, s_0)\).
threeBitAdder :: Vec 3 Bit -> Vec 3 Bit -> Vec 4 Bit
threeBitAdder a b = nBitAdder a b

nBitAdder :: Vec n Bit -> Vec n Bit -> Vec (n+1) Bit
nBitAdder a b = cOut :> s
  where
    tuples =  zip a b

    (cOut,s) = mapAccumR helper 0 tuples

    helper cIn (a',b') = fullAdder a' b' cIn

    fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
    fullAdder a' b' cIn' = (cOutFA, sFA)
      where
        sFA = a' `xor` b' `xor` cIn'
        cOutFA = (cIn' .&. (a' `xor` b')) .|. (a' .&. b')
