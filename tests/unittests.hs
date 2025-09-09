import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import qualified Tests.AHD.Util
import qualified Tests.AHD.Labs.Lab1
import qualified Tests.AHD.Labs.Lab2
import qualified Tests.AHD.Labs.Lab3

import Clash.Prelude

main :: (KnownDomain System) =>  IO ()
main = defaultMain $ testGroup " AHD Clash Lab"
  [ Tests.Example.Project.accumTests,
    Tests.AHD.Util.utilTests,
    Tests.AHD.Labs.Lab1.lab1Tests,
    Tests.AHD.Labs.Lab2.lab2Tests,
    Tests.AHD.Labs.Lab3.lab3Tests
  ]

