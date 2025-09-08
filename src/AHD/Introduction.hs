module AHD.Introduction where

import Clash.Prelude


halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (cOut, s)
  where
    cOut = a .&. b
    s = a `xor` b

topEntity :: Bit -> Bit -> (Bit, Bit)
topEntity = halfAdder








-- TODO was will ich  zeigen
-- interaktiv
-- Index
-- Vec / BitVec
-- pack / Datentypen
