

module Test.Lang.Lens (tests_Lens) where


import Elea.Lang.Lens
import Elea.Lang.Types

import Test.Prelude
import Test.Lang.Types

import qualified Data.HashSet as Set


{-
get ∷ Lens → Val → Maybe Val
get (Lens_Set  setLens ) (Val_Set  set ) = getFromSet   setLens  set
get (Lens_Pair pairLens) (Val_Pair pair) = getFromPair  pairLens pair
get (Lens_Arr  arrLens ) (Val_Arr  arr ) = getFromArray arrLens  arr
get Lens_This             val            = Just val
get _                    _               = Nothing
-}


tests_Lens = testGroup "Val Lenses" [
  
    testCase "Identity Lens 1" $
      get Lens_This (Val_Num $ Z 5) @?= (Just $ Val_Num $ Z 5)
    
  , testCase "Identity Lens 2" $
      get Lens_This complexSet @?= (Just $ complexSet)

  , testCase "First in Pair" $
          get (Lens_Pair $ AtFirst Lens_This) simplePair
      @?= (Just sym_gold)
  
  , testCase "Second in Pair" $
          get (Lens_Pair $ AtSecond Lens_This) simplePair
      @?= (Just $ Val_Num $ Z 100)

  , testCase "Both in Pair" $
          get (Lens_Pair $ AtBoth 
                (Lens_Set $ AnySuchThat (Ty_Num $ IsNumber $ R 1.11)
                                        (Lens_This))
                (Lens_Pair $ AtFirst Lens_This)
              ) 
              complexPair
      @?= (Just $ Val_Pair $ Pair (Val_Num $ R 1.11) sym_blue )

  , testCase "First in Set, such that is 5" $
          get (Lens_Set $ AnySuchThat (Ty_Num $ IsNumber $ Z 5) 
                                      (Lens_This)) 
              primeSet 
      @?= (Just $ Val_Num $ Z 5)

  , testCase "Any in Set, greater than 13" $
          get (Lens_Set $ AnySuchThat (Ty_Num $ GreaterThan $ Z 13)
                                      Lens_This) 
              primeSet 
      @?= (Just $ Val_Num $ Z 17)

  , testCase "Any in Set, greater than 13" $
          get (Lens_Set $ AnySuchThat (Ty_Num $ GreaterThan $ Z 13)
                                      Lens_This) 
              primeSet 
      @?= (Just $ Val_Num $ Z 17)

  , testCase "All in Set, greater than 7" $
          get (Lens_Set $ AllSuchThat (Ty_Num $ GreaterThan $ Z 7)
                                      (Lens_This))
              primeSet 
      @?= (Just $ Val_Set $ Set $ Set.fromList [
              Val_Num $ Z 11, Val_Num $ R 13.0, Val_Num $ Z 17])

  , testCase "Get 2nd item in Array" $
          get (Lens_Arr $ AtIndex 2 Lens_This) simpleArray
      @?= (Just $ Val_Text $ Text "Lisa")
  
    --TODO add tests with nested lenses
  ]

