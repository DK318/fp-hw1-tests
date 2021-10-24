import T2Spec

import Test.Tasty

-- comment  this import and advancedTests if you want run vase tests
import T2SpecAdvanced


main :: IO ()
main = defaultMain (testGroup "HW1.T3 and HW1.T4" [tests, advancedTests])