module AHD.Introduction where

import Clash.Prelude


halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (cOut, s)
  where
    cOut = a .&. b
    s = a `xor` b
topEntity = halfAdder






fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    s = a `xor` b `xor` cIn
    cOut = (cIn .&. (a `xor` b)) .|. (a .&. b)


-- TODO was will ich  zeigen
-- interaktiv
-- Index
-- Vec / BitVec
-- pack / Datentypen
