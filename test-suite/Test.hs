import Test.Tasty

import qualified Math.NumberTheory.LogarithmsTests as Logarithms
import qualified Math.NumberTheory.Powers.IntegerTests as PowerInteger
import qualified Math.NumberTheory.Powers.NaturalTests as PowerNatural

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All"
    [ Logarithms.testSuite
    , PowerInteger.testSuite
    , PowerNatural.testSuite
    ]
