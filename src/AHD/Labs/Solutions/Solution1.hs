module AHD.Labs.Solutions.Solution1 where

import Clash.Prelude


fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    s = a `xor` b `xor` cIn
    cOut = (cIn .&. (a `xor` b)) .|. (a .&. b)
