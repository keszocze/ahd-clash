module Tests.AHD.Labs.Lab1 where

import Prelude
import Clash.Prelude

import AHD.Labs.Lab1

import Tests.AHD.Util
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH


case_Full_Adder :: Assertion
case_Full_Adder = tt3Helper fullAdder [(0,0),(0,1),(0,1),(1,0),(0,1),(1,0),(1,0),(1,1)]

case_Three_Bit_Adder :: Assertion
case_Three_Bit_Adder = do
  mapM_ assertion inputs
    where
      range = [0 :: BitVector 3 .. maxBound]
      inputs = [(a,b) | a <- range, b <- range]

      splitBV :: BitVector 4 -> (Bit, Bit, Bit, Bit)
      splitBV bv = (bv!3, bv!2, bv!1, bv!0)

      assertion :: (BitVector 3, BitVector 3) -> IO ()
      assertion (a, b) = threeBitAdder (a!2) (a!1) (a!0) (b!2) (b!1) (b!0) @=? splitBV (a `add` b)


lab1Tests :: TestTree
lab1Tests = $(testGroupGenerator)


main :: IO ()
main = defaultMain lab1Tests
