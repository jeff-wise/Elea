

module Test.Elea.Lang.Lens (tests_Lens) where



import Test.Prelude
import Test.Data.Val
import Test.Data.System

import Elea.Lang.Lens
import Elea.Lang.Types


import qualified Data.HashSet as Set




tests_Lens = testGroup "Val Lenses" [
  
    testCase "Identity Lens 1" $
      get Lens_This (Val_Num $ Z 5) @?= (Just $ Val_Num $ Z 5)
    
  , testCase "Identity Lens 2" $
      get Lens_This complexSet @?= (Just $ complexSet)

  , testCase "First in Pair" $
          get (getFromFst Lens_This) simplePair
      @?= (Just $ Val_Sym $ sym_rpg^.attr.gold)
  
  , testCase "Second in Pair" $
          get (getFromSnd Lens_This) simplePair
      @?= (Just $ Val_Num $ Z 100)

  , testCase "Both in Pair" $
          get (getFromBoth
                (Lens_Set $ AnySuchThat (Ty_Num $ IsNumber $ R 1.11)
                                        (Lens_This))
                (getFromFst Lens_This)
              )
              complexPair
      @?= (Just $ pair  (Val_Num $ R 1.11) 
                        (Val_Sym $ sym_color^.blue))

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
          get (atIndex (Z 2) Lens_This) simpleArray
      @?= (Just $ Val_Text $ Text "Lisa")
  
    --TODO add tests with nested lenses
  ]

