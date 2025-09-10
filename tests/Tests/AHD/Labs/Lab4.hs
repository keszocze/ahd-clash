{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.AHD.Labs.Lab4 where

import Prelude hiding (foldl, minimum, maximum)
import Clash.Prelude hiding (not, (||))

import AHD.Labs.Lab4

import Clash.Hedgehog.Sized.Unsigned

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog ((===))



case_Three_counter_example :: Assertion
case_Three_counter_example = expected @=? result
  where
    input = [1,3,2,3,0,1,3,0,2,1,2,3,3,3,3,3,3,3,3,3,3,3,3]
    expected = [0,1,1,2,2,2,3,3,3,3,3,4,0,1,2]
    result = simulateN 15 threeCounter input

prop_Three_counter_random :: H.Property
prop_Three_counter_random = H.property $ do
  input <- H.forAll $ Gen.list (Range.linear 1 50) (genUnsigned @_ @2 Range.linearBounded)
  let
    simDuration = Prelude.length input
    reference = simulateN simDuration mythreeCounter input
    result = simulateN simDuration threeCounter input
  result === reference
  where
    mythreeCounter' :: Unsigned 8 -> Unsigned 2 -> (Unsigned 8, Unsigned 3)
    mythreeCounter' cnt input = (cnt', resize $ cnt' `mod` 5)
      where cnt' = if input == 3 then cnt + 1 else cnt

    mythreeCounter :: (HiddenClockResetEnable System) => Signal System (Unsigned 2) -> Signal System (Unsigned 3)
    mythreeCounter = mealy mythreeCounter' 0

lab4Tests ::  TestTree
lab4Tests =  $(testGroupGenerator)


main :: (KnownDomain System) => IO ()
main = defaultMain lab4Tests
