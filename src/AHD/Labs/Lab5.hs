module AHD.Labs.Lab5 where

import AHD.Util
import Clash.Prelude

type Accumulator = Signed 8

data Command =
      Add | Addi (Signed 8) |
      Sub | Subi (Signed 8) |
      Mul | Muli (Signed 8) |
      Jmp | Jmpi (Signed 8) |
      NOP
      deriving (Show, Generic, NFDataX, BitPack)



alu :: Accumulator -> Command -> Accumulator
alu acc Add = acc + acc
alu acc (Addi v) = acc + v
alu _ Sub = 0
alu acc (Subi v) = acc - v
alu acc Mul = acc*acc
alu acc (Muli v) = acc*v
alu acc Jmp = acc
alu acc _ = acc

parseCmd :: BitVector 12 -> Command
parseCmd = undefined
