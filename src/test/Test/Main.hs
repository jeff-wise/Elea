

module Main where


import Test.Prelude

import Test.Elea.Index.Val (tests_ValIndex)
import Test.Elea.Lang.Lens (tests_Lens)
import Test.Elea.Lang.Synthesis (tests_Synthesis)
import Test.Elea.Lang.Type (tests_Type)
import Test.Elea.Lang.Val (tests_Val)




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
  , tests_ValIndex
  ]
