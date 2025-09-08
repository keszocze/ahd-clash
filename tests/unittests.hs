import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import qualified Tests.AHD.Util
import qualified Tests.AHD.Labs.Lab1

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Example.Project.accumTests,
    Tests.AHD.Util.utilTests,
    Tests.AHD.Labs.Lab1.lab1Tests
  ]
