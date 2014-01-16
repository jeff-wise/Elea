

module Main where


import Test.Prelude

import Test.Elea.Lang.Index.Val (tests_ValIndex)
import Test.Elea.Lang.Index.Type (tests_TypeIndex)
import Test.Elea.Lang.Atom.Lens (tests_Lens)
import Test.Elea.Lang.Atom.Type (tests_Type)
import Test.Elea.Lang.Atom.Val (tests_Val)




main ∷ IO ()  
main = defaultMain tests


tests ∷ TestTree
tests = testGroup "Tests" [unitTests]


unitTests ∷ TestTree
unitTests = testGroup "Unit Tests" [
    tests_Lens 
  , tests_Type
  , tests_Val
--  , tests_Synthesis
  , tests_ValIndex
  , tests_TypeIndex
  ]
