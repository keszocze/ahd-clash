import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import qualified Tests.AHD.Util
import qualified Tests.AHD.Labs.Lab1
import qualified Tests.AHD.Labs.Lab2
import qualified Tests.AHD.Labs.Lab3
import qualified Tests.AHD.Labs.Lab4
import qualified Tests.AHD.Labs.Lab5

import Clash.Prelude

main :: (KnownDomain System) =>  IO ()
main = defaultMain $ testGroup " AHD Clash Lab"
  [ Tests.Example.Project.accumTests,
    Tests.AHD.Util.utilTests,
    Tests.AHD.Labs.Lab1.lab1Tests,
    Tests.AHD.Labs.Lab2.lab2Tests,
    Tests.AHD.Labs.Lab3.lab3Tests,
    Tests.AHD.Labs.Lab4.lab4Tests,
    Tests.AHD.Labs.Lab5.lab5Tests
  ]

