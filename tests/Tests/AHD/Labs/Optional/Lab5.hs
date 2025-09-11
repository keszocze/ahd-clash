{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.AHD.Labs.Optional.Lab5 where

import Prelude hiding (foldl, minimum, maximum)
import Clash.Prelude hiding (not, (||))

import AHD.Labs.Lab5


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Range as Range
import Hedgehog ((===))
import Clash.Hedgehog.Sized.BitVector
import Clash.Hedgehog.Sized.Signed (genSigned)

import Debug.Trace


prop_Tripping_from_Command_to_BitVector_and_back_Addi :: H.Property
prop_Tripping_from_Command_to_BitVector_and_back_Addi = trippingHelperCmd2BV2CMD Addi

prop_Tripping_from_Command_to_BitVector_and_back_Subi :: H.Property
prop_Tripping_from_Command_to_BitVector_and_back_Subi = trippingHelperCmd2BV2CMD Subi
prop_Tripping_from_Command_to_BitVector_and_back_Muli :: H.Property
prop_Tripping_from_Command_to_BitVector_and_back_Muli = trippingHelperCmd2BV2CMD Muli
prop_Tripping_from_Command_to_BitVector_and_back_Jmpi :: H.Property
prop_Tripping_from_Command_to_BitVector_and_back_Jmpi = trippingHelperCmd2BV2CMD Jmpi

case_Tripping_from_Command_to_BitVector_and_back_Add :: Assertion
case_Tripping_from_Command_to_BitVector_and_back_Add = parseCmd (pack Add) @=? Add
case_Tripping_from_Command_to_BitVector_and_back_Sub :: Assertion
case_Tripping_from_Command_to_BitVector_and_back_Sub = parseCmd (pack Sub) @=? Sub
case_Tripping_from_Command_to_BitVector_and_back_Mul :: Assertion
case_Tripping_from_Command_to_BitVector_and_back_Mul = parseCmd (pack Mul) @=? Mul
case_Tripping_from_Command_to_BitVector_and_back_Jmp :: Assertion
case_Tripping_from_Command_to_BitVector_and_back_Jmp = parseCmd (pack Jmp) @=? Jmp
case_Tripping_from_Command_to_BitVector_and_back_NOP :: Assertion
case_Tripping_from_Command_to_BitVector_and_back_NOP = parseCmd (pack NOP) @=? NOP
case_Tripping_from_Command_to_BitVector_and_back_Stop :: Assertion
case_Tripping_from_Command_to_BitVector_and_back_Stop = parseCmd (pack Stop) @=? Stop

trippingHelperCmd2BV2CMD :: KnownNat n => (Signed n -> Command) -> H.Property
trippingHelperCmd2BV2CMD cmdCtr = H.property $ do
  v <- H.forAll $ genSigned Range.linearBounded
  let cmd = cmdCtr v
  parseCmd (pack cmd) === cmd

prop_Tripping_from_BitVector_to_Command_and_back_Add :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Add = trippingHelperBV2Cmd2BV 0b0000
prop_Tripping_from_BitVector_to_Command_and_back_Addi :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Addi = trippingHelperBV2Cmd2BV 0b0001
prop_Tripping_from_BitVector_to_Command_and_back_Sub :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Sub = trippingHelperBV2Cmd2BV 0b0010
prop_Tripping_from_BitVector_to_Command_and_back_Subi :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Subi = trippingHelperBV2Cmd2BV 0b0011
prop_Tripping_from_BitVector_to_Command_and_back_Mul :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Mul = trippingHelperBV2Cmd2BV 0b0100
prop_Tripping_from_BitVector_to_Command_and_back_Muli :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Muli = trippingHelperBV2Cmd2BV 0b0101
prop_Tripping_from_BitVector_to_Command_and_back_Jmp :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Jmp = trippingHelperBV2Cmd2BV 0b0110
prop_Tripping_from_BitVector_to_Command_and_back_Jmpi :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Jmpi = trippingHelperBV2Cmd2BV 0b0111
prop_Tripping_from_BitVector_to_Command_and_back_NOP :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_NOP = trippingHelperBV2Cmd2BV 0b1000
prop_Tripping_from_BitVector_to_Command_and_back_Stop :: H.Property
prop_Tripping_from_BitVector_to_Command_and_back_Stop = trippingHelperBV2Cmd2BV 0b1001

prop_Parsing__to_NOP_from_garbage_input :: H.Property
prop_Parsing__to_NOP_from_garbage_input = H.property $ do
  bv <- H.forAll $ genDefinedBitVector @_ @8
  mapM_ (\prefix ->
      (parseCmd (prefix ++# bv)) === NOP
    )
    [0b1010 .. 0b111]

trippingHelperBV2Cmd2BV :: BitVector 4 -> H.Property
trippingHelperBV2Cmd2BV prefix = H.property $ do
  bv <- H.forAll $ genDefinedBitVector @_ @8
  let
    cmdBV =  prefix ++# bv
    cmdCommand =  parseCmd cmdBV
    packedBV =  pack cmdCommand
  H.footnote $ show cmdBV <> " -> " <> show cmdCommand <> " -> " <> show packedBV
  H.assert (isLike cmdBV packedBV)


lab5Tests ::  TestTree
lab5Tests =  $(testGroupGenerator)


main :: (KnownDomain System) => IO ()
main = defaultMain lab5Tests
