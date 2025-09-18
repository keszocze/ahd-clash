{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.AHD.Labs.Lab3 where

import Prelude hiding (foldl, minimum, maximum)
import Clash.Prelude hiding (not, (||))

import AHD.Labs.Lab3

import Clash.Hedgehog.Sized.Unsigned
import Clash.Class.Counter

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog ((===))



prop_Delay :: H.Property
prop_Delay = H.property $ do
  input <- H.forAll $ Gen.list (Range.linear 1 50) (genUnsigned @_ @8 Range.linearBounded)
  let
      simDuration = Prelude.length input
      result = simulateN simDuration delay2 input
      reference = simulateN simDuration myDelay2 input
  result === reference
    where
      myDelay2 :: SystemClockResetEnable => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
      myDelay2 v = v''
        where
          v' = register 42 v
          v'' = register 42 v'


prop_Modify_values_zero_if_even :: H.Property
prop_Modify_values_zero_if_even = H.property $ do
  input <- H.forAll $ Gen.list (Range.linear 1 50) (genUnsigned @_ @8 Range.linearBounded)
  let
      simDuration = Prelude.length input
      result = simulateN simDuration zeroIfEven input
      reference = simulateN @System simDuration myZeroIfEven input
  result === reference
    where
      myZeroIfEven = fmap (\v -> if Clash.Prelude.even v then 0 else v)

prop_Modify_values_times_two_plus_three :: H.Property
prop_Modify_values_times_two_plus_three = H.property $ do
  input <- H.forAll $ Gen.list (Range.linear 1 50) (genUnsigned @_ @8 Range.linearBounded)
  let
      simDuration = Prelude.length input
      result = simulateN simDuration timesTwoPlusThree input
      reference = simulateN @System simDuration (\s -> 2*s + 3) input
  result === reference

testClock :: forall n m. (KnownNat n, KnownNat m) => H.Property
testClock = H.property $ do
  let
    n' = natToInteger @n
    m' = natToInteger @m
    nMax' = 2 Prelude.^ n'
    mMax' = 2 Prelude.^ m'
  nMax <- H.forAll $ Gen.int (Range.linear 0 nMax')
  mMax <- H.forAll $ Gen.int (Range.linear 0 mMax')
  let
    simDuration = 2*(nMax + mMax)
    result = sampleN simDuration (clock @n @m)
    reference = sampleN simDuration myClock
  result === reference
    where
      myClock :: (SystemClockResetEnable, KnownNat n, KnownNat m) =>  Signal System (Unsigned n, Unsigned m)
      myClock = r
        where
          r = register (0, 0) (fmap countSucc r)

test_Clock :: [TestTree]
test_Clock = [
  testProperty "Test the clock with n=2, m=4" (testClock @2 @4),
  testProperty "Test the clock with n=2, m=8" (testClock @2 @8),
  testProperty "Test the clock with n=7, m=4" (testClock @7 @4),
  testProperty "Test the clock with n=1, m=3" (testClock @1 @3),
  testProperty "Test the clock with n=2, m=3" (testClock @2 @2),
  testProperty "Test the clock with n=3, m=3" (testClock @3 @2)
  ]


lab3Tests ::  TestTree
lab3Tests =  $(testGroupGenerator)


main :: (KnownDomain System) => IO ()
main = defaultMain lab3Tests
