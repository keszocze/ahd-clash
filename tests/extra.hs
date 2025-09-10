import Prelude

import Test.Tasty


import qualified Tests.AHD.Labs.ELab4

import Clash.Prelude

main :: (KnownDomain System) =>  IO ()
main = defaultMain $ testGroup " AHD Clash Lab -- Optional Tests"
  [
    Tests.AHD.Labs.ELab4.lab4Tests
  ]

