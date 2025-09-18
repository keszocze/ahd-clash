module Tests.AHD.Util (tt2Helper, tt3Helper, main, utilTests) where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Clash.Prelude hiding (drop)

import AHD.Util

tt2Helper :: (Eq a, Show a) => (Bit -> Bit -> a) -> [a] -> Assertion
tt2Helper f expected = do
  let vals = truthTable2 f
  Prelude.length vals @?= 4
  vals @?= expected

tt3Helper :: (Eq a, Show a) => (Bit -> Bit -> Bit -> a) -> [a] -> Assertion
tt3Helper f expected = do
  let vals = truthTable3 f
  Prelude.length vals @=? 8
  vals @=? expected

case_tt2And :: Assertion
case_tt2And = tt2Helper(.&.) [0,0,0,1]
case_tt2Xor :: Assertion
case_tt2Xor = tt2Helper xor [0,1,1,0]
case_tt2Or :: Assertion
case_tt2Or = tt2Helper (.|.) [0,1,1,1]

case_Generating_all_TwoBit_inputs :: Assertion
case_Generating_all_TwoBit_inputs = do
  let vals = drop 1 $ sampleN 9 bitInputs2
      expected = [(0,0), (0,1), (1,0), (1,1), (0,0), (0,1), (1,0), (1,1)]
  vals @?= expected

case_Generating_all_ThreeBit_inputs :: Assertion
case_Generating_all_ThreeBit_inputs = do
  let vals = drop 1 $ sampleN 9 bitInputs3
      expected = [(0,0,0), (0,0,1), (0,1,0), (0,1,1), (1,0,0), (1,0,1), (1,1,0), (1,1,1)]
  vals @?= expected

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    s = a `xor` b `xor` cIn
    cOut = (cIn .&. (a `xor` b)) .|. (a .&. b)


case_tt3FullAdder :: Assertion
case_tt3FullAdder = tt3Helper fullAdder [(0,0),(0,1),(0,1),(1,0),(0,1),(1,0),(1,0),(1,1)]

case_tt3SomeFun :: Assertion
case_tt3SomeFun = tt3Helper (\a b c -> (a `xor` b) .|. c) [0,1,1,1,1,1,0,1]

utilTests :: TestTree
utilTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain utilTests
