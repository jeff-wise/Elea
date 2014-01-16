

module Test.Data.Val (
    primeSet, complexSet
  , simplePair, complexPair
  , simpleArray
  ) where


import Test.Prelude
import Test.Data.System

import Elea.Lang.Atom.Types


import qualified Data.HashSet as Set
import qualified Data.Sequence as Seq



---------------------------------------------------------------------
-- Basic Vals
---------------------------------------------------------------------

primeSet ∷ Val
primeSet = Val_Set $ Set $ Set.fromList [
    Val_Num $ Z 2
  , Val_Num $ Z 3
  , Val_Num $ Z 5
  , Val_Num $ Z 7
  , Val_Num $ Z 11
  , Val_Num $ Z 13
  , Val_Num $ Z 17
  ]


complexSet ∷ Val
complexSet = Val_Set $ Set $ Set.fromList [
    Val_Num $ Z 1
  , Val_Text $ Text "A Set Element"
  , pair (Val_Sym $ sym_social^.friends) (Val_Num $ Z 500)
  , Val_Set $ Set $ Set.fromList [
        Val_Num $ Z 100
      , Val_Text $ Text "One-hundred"
      , Val_Arr $ Arr $ Seq.fromList 
          [Val_Num $ R 1.414, Val_Num $ R 3.14]
      ]
  , simpleArray
  ]



simplePair ∷ Val
simplePair = pair (Val_Sym $ sym_color^.blue)
                  (Val_Num $ Z 100)


complexPair ∷ Val
complexPair = pair
    (Val_Set $ Set $ Set.fromList [ Val_Num $ Z 3
                                  , Val_Num $ R 1.11
                                  , Val_Text $ Text "Bird"
                                  ]
    ) 
    (pair (Val_Sym $ sym_color^.blue) simpleArray) 


simpleArray ∷ Val
simpleArray = Val_Arr $ Arr $ Seq.fromList [
    Val_Text $ Text "Joe"
  , Val_Text $ Text "Bob" 
  , Val_Text $ Text "Lisa"
  , Val_Text $ Text "Mike"
  ]



