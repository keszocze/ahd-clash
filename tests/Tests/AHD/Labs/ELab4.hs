{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.AHD.Labs.ELab4 where

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
import Clash.Hedgehog.Sized.BitVector (genBit)

import Debug.Trace

case_Clock_example :: Assertion
case_Clock_example = expected @=? result
  where
    input = 0 : Prelude.repeat 1
    expected = [(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3),(0,0)]
    result = simulateN 13 (clock @2 @3 2 3) input

prop_Clock_random :: H.Property
prop_Clock_random = H.property $ do
  input <- H.forAll $ Gen.list (Range.linear 5 50) genBit
  kUpper <- H.forAll $ genUnsigned (Range.linear 1 15)
  kLower <- H.forAll $ genUnsigned (Range.linear 1 7)
  let
    simDuration = Prelude.length (traceShowId input)
    reference = traceShowId $ simulateN simDuration (myclock @4 @3 kUpper $ traceShowId kLower) input
    result = traceShowId $ simulateN simDuration (clock @4 @3 kUpper $ traceShowId kLower) input
  result === reference
  where
      mycounter :: (KnownNat n) => Unsigned n -> Unsigned n -> Bit -> (Unsigned n, (Bit, Unsigned n))
      mycounter k s advance =
        if advance == 1
          then (s', (advanceNext, s'))
          else (s, (0, s))
        where
            (advanceNext, s')  = if s == k then (1,0) else (0,s+1)

      mylowerClock :: (HiddenClockResetEnable System, KnownNat m) => Unsigned m -> Signal System Bit -> Signal System (Bit, Unsigned m)
      mylowerClock k = mealy @System (mycounter k) 0


      myupperClock :: (HiddenClockResetEnable System, KnownNat n) => Unsigned n -> Signal System Bit -> Signal System (Bit, Unsigned n)
      myupperClock k = mealy @System (mycounter k) 0


      myclock :: (HiddenClockResetEnable System, KnownNat n, KnownNat m) => Unsigned n -> Unsigned m -> Signal System Bit -> Signal System (Unsigned n, Unsigned m)
      myclock kUpper kLower i = bundle (lClock, rClock)
            where
                  (wrap,rClock) = unbundle $ (mylowerClock kLower) i
                  (_, lClock) = unbundle $ (myupperClock kUpper) wrap


lab4Tests ::  TestTree
lab4Tests =  $(testGroupGenerator)


main :: (KnownDomain System) => IO ()
main = defaultMain lab4Tests
