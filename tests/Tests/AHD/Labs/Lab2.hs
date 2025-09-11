{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.AHD.Labs.Lab2 where

import Prelude hiding (minimum, maximum)
import Clash.Prelude

import AHD.Labs.Lab2

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Range as Range
import Hedgehog ((===))


case_Three_Bit_Adder :: Assertion
case_Three_Bit_Adder = mapM_ assertion inputs
    where
      range = [0 :: BitVector 3 .. maxBound]
      inputs = [(a,b) | a <- range, b <- range]

      assertion :: (BitVector 3, BitVector 3) -> Assertion
      assertion (a, b) = threeBitAdder (bv2v a) (bv2v b) @=? bv2v (a `add` b)

adderTester :: forall n. (KnownNat n) => Assertion
adderTester = mapM_ assertion inputs
    where
      range = [0 :: BitVector n .. maxBound]
      inputs = [(a,b) | a <- range, b <- range]

      assertion :: (BitVector n, BitVector n) -> Assertion
      assertion (a, b) = nBitAdder (bv2v a) (bv2v b) @=? bv2v (a `add` b)

case_Two_Bit_Adder :: Assertion
case_Two_Bit_Adder = adderTester @2

case_Four_Bit_Adder :: Assertion
case_Four_Bit_Adder = adderTester @4

case_Eight_Bit_Adder :: Assertion
case_Eight_Bit_Adder = adderTester @8



capRandomT :: forall n. (KnownNat n) => H.Property
capRandomT = H.property $ do
  vec <- H.forAll (genVec @_ @n (genUnsigned Range.linearBounded))
  t <- H.forAll $ genUnsigned Range.linearBounded
  let result = capAt t vec
  H.assert $ all (<= t) result


capAtZero :: forall n. (KnownNat n) => H.Property
capAtZero = H.property $ do
  vec <- H.forAll (genVec @_ @n (genUnsigned Range.linearBounded))
  let result = capAt 0 vec
  H.assert $ all (== 0) result

getMinMax :: forall n k. (KnownNat n, (k + 1) ~ n) => H.Property
getMinMax = H.property $ do
  vec <- H.forAll (genVec @_ @n (genUnsigned Range.linearBounded))
  let (resMin,resMax) = minMax vec
  minimum @(Unsigned 8) @k vec === resMin
  maximum @(Unsigned 8) @k vec === resMax


capTests :: TestTree
capTests = testGroup "Capping values" [
    testProperty "Cap at random value (vec len = 4)" (capRandomT @4),
    testProperty "Cap at random value (vec len = 16)" (capRandomT @16),
    testProperty "Cap at random value (vec len = 32)" (capRandomT @32),
    testProperty "Cap at zero (vec len = 4)" (capAtZero @4),
    testProperty "Cap at zero (vec len = 16)" (capAtZero @16),
    testProperty "Cap at zero (vec len = 32)" (capAtZero @32)
  ]

minMaxTests :: TestTree
minMaxTests = testGroup "Getting the min/max" [
    testProperty "Getting random min/max values (vec len = 4)" (getMinMax @4),
    testProperty "Getting random min/max values (vec len = 16)" (getMinMax @16),
    testProperty "Getting random min/max values (vec len = 32)" (getMinMax @32),
    testProperty "Getting random min/max values (vec len = 50)" (getMinMax @50)
  ]


adderTests :: TestTree
adderTests = $(testGroupGenerator)


lab2Tests :: TestTree
lab2Tests = testGroup "Lab2" [
   capTests, minMaxTests, adderTests
  ]

main :: IO ()
main = defaultMain lab2Tests
