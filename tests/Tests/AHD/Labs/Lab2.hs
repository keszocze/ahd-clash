{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.AHD.Labs.Lab2 where

import Prelude
import Clash.Prelude

import AHD.Labs.Lab2


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_Three_Bit_Adder :: Assertion
case_Three_Bit_Adder = do
  mapM_ assertion inputs
    where
      range = [0 :: BitVector 3 .. maxBound]
      inputs = [(a,b) | a <- range, b <- range]

      assertion :: (BitVector 3, BitVector 3) -> IO ()
      assertion (a, b) = threeBitAdder (bv2v a) (bv2v b) @=? bv2v (a `add` b)

adderTester :: forall n. (KnownNat n) => Assertion
adderTester = do
  mapM_ assertion inputs
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




lab2Tests :: TestTree
lab2Tests = $(testGroupGenerator)


main :: IO ()
main = defaultMain lab2Tests
