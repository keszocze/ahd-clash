{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Tests.AHD.Labs.Lab5 where

import AHD.Labs.Lab5
import Clash.Hedgehog.Sized.Signed
import Clash.Prelude hiding (not, (||))
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude hiding (foldl, maximum, minimum, repeat, (++))

prop_ALU_Add :: H.Property
prop_ALU_Add = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc + initialAcc
      result = alu initialAcc Add
  expected === result

prop_ALU_Addi :: H.Property
prop_ALU_Addi = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  immediate <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc + immediate
      result = alu initialAcc (Addi immediate)
  expected === result

prop_ALU_Addi_zero_immediate :: H.Property
prop_ALU_Addi_zero_immediate = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc
      result = alu initialAcc (Addi 0)
  expected === result

prop_ALU_Addi_zero_acc :: H.Property
prop_ALU_Addi_zero_acc = H.property $ do
  immediate <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = immediate
      result = alu 0 (Addi immediate)
  expected === result

prop_ALU_Sub :: H.Property
prop_ALU_Sub = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = 0
      result = alu initialAcc Sub
  expected === result

prop_ALU_Subi :: H.Property
prop_ALU_Subi = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  immediate <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc - immediate
      result = alu initialAcc (Subi immediate)
  expected === result

prop_ALU_Subi_zero_immediate :: H.Property
prop_ALU_Subi_zero_immediate = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc
      result = alu initialAcc (Subi 0)
  expected === result

prop_ALU_Subi_zero_acc :: H.Property
prop_ALU_Subi_zero_acc = H.property $ do
  immediate <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = (-immediate)
      result = alu 0 (Subi immediate)
  expected === result

prop_ALU_Mul :: H.Property
prop_ALU_Mul = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc * initialAcc
      result = alu initialAcc Mul
  expected === result

prop_ALU_Muli :: H.Property
prop_ALU_Muli = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  immediate <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc * immediate
      result = alu initialAcc (Muli immediate)
  expected === result

prop_ALU_NOPs :: H.Property
prop_ALU_NOPs = H.property $ do
  initialAcc <- H.forAll $ genSigned @_ @8 Range.linearBounded
  immediate <- H.forAll $ genSigned @_ @8 Range.linearBounded
  let expected = initialAcc
      resultJmp = alu initialAcc Jmp
      resultJmpi = alu initialAcc (Jmpi immediate)
      resultNOP = alu initialAcc NOP
      resultStop = alu initialAcc Stop
  expected === resultJmp
  expected === resultJmpi
  expected === resultNOP
  expected === resultStop

-- | Returns the PC value after a specific number of clock cycles have passed
getPC ::
  -- | How many clock cycles should have passed
  Int ->
  -- | The program to run
  Vec 64 Command ->
  PC
getPC n inp = let (_, pc, _) = (Prelude.last $ sampleN (n + 2) (accMachine inp)) in pc

test_immediate_jumps :: [TestTree]
test_immediate_jumps =
  [ testCase "Line 1 in right-hand side of table" (getPC 1 (mkCmds $ singleton $ Jmpi 0) @?= 0),
    testCase "Line 2 in right-hand side of table" (getPC 1 (mkCmds $ singleton $ Jmpi 1) @?= 1),
    testCase "Line 3 in right-hand side of table" (getPC 1 (mkCmds $ singleton $ Jmpi (-1)) @?= 63),
    testCase "Line 4 in right-hand side of table" (getPC 1 (mkCmds $ singleton $ Jmpi 65) @?= 1),
    testCase "Line 5 in right-hand side of table" (getPC 64 ((repeat @63 NOP) ++ ((Jmpi 1) :> Nil)) @?= 0),
    testCase "Line 6 in right-hand side of table" (getPC 1 (mkCmds $ singleton $ Jmpi (-4)) @?= 60)
  ]

test_jumps :: [TestTree]
test_jumps =
  [ testCase "Line 1 in left-hand side of table" (getPC 1 (mkCmds $ singleton $ Jmp) @?= 0),
    testCase "Line 2 in left-hand side of table" (getPC 2 (mkCmds $ (Addi 63) :> Jmp :> Nil) @?= 63),
    testCase "Line 3 in left-hand side of table" (getPC 2 (mkCmds $ (Addi 64) :> Jmp :> Nil) @?= 0),
    testCase "Line 4 in left-hand side of table" (getPC 2 (mkCmds $ (Addi 65) :> Jmp :> Nil) @?= 1),
    testCase "Line 5 in left-hand side of table" (getPC 2 (mkCmds $ (Addi (-1)) :> Jmp :> Nil) @?= 63),
    testCase "Line 6 in left-hand side of table" (getPC 2 (mkCmds $ (Addi (-2)) :> Jmp :> Nil) @?= 62)
  ]

checkAC :: (KnownNat n, KnownNat k, n <= 64, n + k ~ 64) => Int -> Vec n Command -> [(Accumulator, PC, Command)] -> Assertion
checkAC n input expected = result @?= expected
  where
    result = sampleN n (accMachine input')
    input' = mkCmds input

-- TODO mehr Testfälle hinzufügen

case_Example_from_the_Testing_the_machine_section :: Assertion
case_Example_from_the_Testing_the_machine_section =
  checkAC
    10
    ((Addi 10) :> NOP :> (Jmpi (-2)) :> NOP :> Nil)
    [ (0, 0, Addi 10),
      (0, 0, Addi 10),
      (10, 1, NOP),
      (10, 2, Jmpi (-2)),
      (10, 0, Addi 10),
      (20, 1, NOP),
      (20, 2, Jmpi (-2)),
      (20, 0, Addi 10),
      (30, 1, NOP),
      (30, 2, Jmpi (-2))
    ]

lab5Tests :: TestTree
lab5Tests = $(testGroupGenerator)

main :: (KnownDomain System) => IO ()
main = defaultMain lab5Tests
