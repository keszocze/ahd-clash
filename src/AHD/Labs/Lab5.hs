{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AHD.Labs.Lab5 where

import AHD.Util
import Clash.Prelude

-- | Alias for the accumulator value
type Accumulator = Signed 8

-- | Alias for the program counter
--
-- We only support programs with a lenght of up to 64
type PC = Unsigned 6

data Command
  = -- | Add the accumulator to itself
    Add
  | -- | Add immediate to the accumulator
    Addi Accumulator
  | -- | Subtract the accumulator from itself
    Sub
  | -- | Subtract immedate from the accumulator
    Subi Accumulator
  | -- | Multiply the accumulator with itself
    Mul
  | -- | Multiply the accumulator with the immediate
    Muli Accumulator
  | -- | Set PC to the accumulator value
    Jmp
  | -- | Set PC to PI + immediate
    Jmpi Accumulator
  | -- | Do nothing (except advancing the PC)
    NOP
  | -- | Stop the accumulator machine
    Stop
  deriving (Show, Eq, Generic, NFDataX, BitPack)

-- | Simple ALU of the accumulator machine
alu ::
  -- | The current acumumulator value
  Accumulator ->
  -- | The command to execute
  Command ->
  -- | The updated accumulator value
  Accumulator
alu acc Add = acc + acc
alu acc (Addi v) = acc + v
alu _ Sub = 0
alu acc (Subi v) = acc - v
alu acc Mul = acc * acc
alu acc (Muli v) = acc * v
alu acc Jmp = acc
alu acc _ = acc

-- | The accumulator machine
--
-- It is parameterized with the read-only memory for the program
accMachine ::
  (HiddenClockResetEnable System) =>
  -- | The read-only program
  Vec 64 Command ->
  -- | Output tuple consisting of
  --
  -- * The current  accumulator value
  -- * The current PC value
  -- * The command that was executed this clock cycle
  Signal System (Accumulator, PC, Command)
accMachine cmds = bundle (acc, pc, cmd)
  where
    acc = register (0 :: Accumulator) (liftA2 alu acc cmd)
    pc = register (0 :: PC) $ liftA3 updPc pc cmd acc
    updPc :: PC -> Command -> Accumulator -> PC
    updPc pc' cmd' acc' = case cmd' of
      Stop -> pc'
      Jmp -> resize $ bitCoerce acc'
      (Jmpi v) -> pc' + bitCoerce (resize $ pack v)
      _ -> pc' + 1
    cmd = fmap (cmds !!) pc

-- | The accumulator machine that only outputs the accumulator values
accMachine' :: (HiddenClockResetEnable System) => Vec 64 Command -> Signal System Accumulator
accMachine' cmds = fmap (\(a, _, _) -> a) (accMachine cmds)

-- | Creates a read-only program vector by appending `NOP`s
--
-- > clashi> mkCmds ((Addi 3) :> Add :> Nil)
-- > Addi 3 :> Add :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> Nil
mkCmds :: (KnownNat n, KnownNat k, n <= 64, n + k ~ 64) => Vec n Command -> Vec 64 Command
mkCmds cmds = cmds ++ (repeat NOP)

-- | Creates a read-only program vector by appending a `Stop` and `NOP`s
--
-- > clashi> mkCmds' ((Addi 3) :> Add :> Nil)
-- > Addi 3 :> Add :> Stop :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> NOP :> Nil
mkCmds' :: (KnownNat n, KnownNat k, n <= 64, n + (k + 1) ~ 64) => Vec n Command -> Vec 64 Command
mkCmds' cmds = cmds ++ (Stop :> (repeat NOP))

-- | Runs a program on the accumulator machine
--
-- It uses `mkCmds` and `prettySampleN`
--
-- > clashi> evalAM 10 ((Addi 10) :> NOP :> (Jmpi (-2)) :> NOP :> Nil)
-- > (0,0,Addi 10)
-- > (0,0,Addi 10)
-- > (10,1,NOP)
-- > (10,2,Jmpi -2)
-- > (10,0,Addi 10)
-- > (20,1,NOP)
-- > (20,2,Jmpi -2)
-- > (20,0,Addi 10)
-- > (30,1,NOP)
-- > (30,2,Jmpi -2)
evalAM :: (KnownNat n, KnownNat k, n <= 64, n + k ~ 64) => Int -> Vec n Command -> IO ()
evalAM n cmds = prettySampleN n (accMachine $ mkCmds cmds)

-- | Runs a program on the accumulator machine
--
-- It uses `mkCmds'` and `prettySampleN`
evalAM' :: (KnownNat n, KnownNat k, n <= 64, n + (k + 1) ~ 64) => Int -> Vec n Command -> IO ()
evalAM' n cmds = prettySampleN n (accMachine $ mkCmds' cmds)

-- | Optional
parseCmd :: BitVector 12 -> Command
parseCmd $(bitPattern "0000_...._....") = Add
parseCmd $(bitPattern "0001_aaaa_aaaa") = Addi $ bitCoerce aaaaaaaa
parseCmd $(bitPattern "0010_...._....") = Sub
parseCmd $(bitPattern "0011_aaaa_aaaa") = Subi $ bitCoerce aaaaaaaa
parseCmd $(bitPattern "0100_...._....") = Mul
parseCmd $(bitPattern "0101_aaaa_aaaa") = Muli $ bitCoerce aaaaaaaa
parseCmd $(bitPattern "0110_...._....") = Jmp
parseCmd $(bitPattern "0111_aaaa_aaaa") = Jmpi $ bitCoerce aaaaaaaa
parseCmd $(bitPattern "1001_...._....") = Stop
parseCmd _ = NOP
