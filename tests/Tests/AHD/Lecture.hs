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
import Tests.AHD.Util

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



lectureTests :: TestTree
lectureTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain lectureTests
