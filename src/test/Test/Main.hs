

module Main where


import Test.Prelude

import Test.Lang.Lens (tests_Lens)
import Test.Lang.Synthesis (tests_Synthesis)
import Test.Lang.Type (tests_Type)
import Test.Lang.Val (tests_Val)




main ∷ IO ()  
main = defaultMain tests


tests ∷ TestTree
tests = testGroup "Tests" [unitTests]


unitTests ∷ TestTree
unitTests = testGroup "Unit Tests" [
    tests_Lens 
  , tests_Type
  , tests_Val
  , tests_Synthesis
  ]
