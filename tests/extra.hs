import Prelude

import Test.Tasty


import qualified Tests.AHD.Labs.Optional.Lab4
import qualified Tests.AHD.Labs.Optional.Lab5

import Clash.Prelude

main :: (KnownDomain System) =>  IO ()
main = defaultMain $ testGroup " AHD Clash Lab -- Optional Tests"
  [
    Tests.AHD.Labs.Optional.Lab4.lab4Tests,
    Tests.AHD.Labs.Optional.Lab5.lab5Tests
  ]

