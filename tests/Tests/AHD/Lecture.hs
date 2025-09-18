module Tests.AHD.Lecture where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Hedgehog.Sized.Unsigned

import Clash.Prelude

import AHD.Lecture

import AHD.Util

case_My_MUX :: Assertion
case_My_MUX = do
  myMux False (2 :: Unsigned 4) 1 @?= 2
  myMux True (2 :: Unsigned 4) 1 @?= 1


prop_My_MUX_random :: H.Property
prop_My_MUX_random = H.property $ do
  a <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
  b <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
  sel <- H.forAll Gen.bool
  let
    result = myMux sel a b
    expected = if sel then b else a
  result === expected

prop_Four_way_Muxes_behave_identical :: H.Property
prop_Four_way_Muxes_behave_identical = H.property $ do
    a <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
    b <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
    c <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
    d <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
    sel <- H.forAll $ genUnsigned @_ @2 Range.linearBounded

    fourWayMuxDirect sel a b c d === fourWayMuxVec sel (a :> b :> c :> d :> Nil)

case_Half_Adder_is_correct :: Assertion
case_Half_Adder_is_correct = truthTable2 halfAdder @?=  [(0,0),(0,1),(0,1),(1,0)]

case_Sequential_Half_Adder_is_correct :: Assertion
case_Sequential_Half_Adder_is_correct = sampleN 5 (seqEval2 seqHalfAdder) @?= [(0,0), (0,0),(0,1),(0,1),(1,0)]

case_Simple_Counter_works :: Assertion
case_Simple_Counter_works = sampleN 20 cnt @?= [0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0,1,2]

case_fmapCntStep_with_step_2 :: Assertion
case_fmapCntStep_with_step_2 = sampleN 20 (fmapCntStep 2) @?= [0,0,2,4,6,0,2,4,6,0,2,4,6,0,2,4,6,0,2,4]

case_fmapCntStep_with_step_3 :: Assertion
case_fmapCntStep_with_step_3 = sampleN 20 (fmapCntStep 3) @?=  [0,0,3,6,1,4,7,2,5,0,3,6,1,4,7,2,5,0,3,6]

case_OneHot_Counter_works :: Assertion
case_OneHot_Counter_works = sampleN 10 oneHotCounter @?= [0b0001,0b0001,0b0010,0b0100,0b1000,0b0001,0b0010,0b0100,0b1000,0b0001]

prop_The_Doubler_doubles :: H.Property
prop_The_Doubler_doubles = H.property $ do
  a <- H.forAll $ genUnsigned @_ @8 Range.linearBounded
  let
    result = simulateN 1 seqDoubler [a]
    expected = [2*a]
  result === expected

case_MyReg_contains_the_correct_values :: Assertion
case_MyReg_contains_the_correct_values = sampleN 8 myReg @?= [3,3,4,4,4,4,4,4]

case_The_parity_is_correctly_computed :: Assertion
case_The_parity_is_correctly_computed = simulateN 10  parityMealy [0,0,1,1,0,1,0,1,0,1] @?= [False,False,True,False,False,True,True,False,False,True]

lectureTests :: TestTree
lectureTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain lectureTests
