

module Main where


import EleaTest.Prelude

import EleaTest.Lang.Sem.ValueIndex
import EleaTest.Lang.Sem.TypeIndex




main ∷ IO ()  
main = defaultMain tests


tests ∷ TestTree
tests = testGroup "Tests"
  [ tests_ValueIndex
  , tests_TypeIndex
  ]


