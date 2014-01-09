


module Elea.Lang.Lens (get) where


import Elea.Prelude
import Elea.Lang.Types
import Elea.Lang.Type (isType)
import Elea.Lang.Val


import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.List.Stream as L



get ∷ Lens → Val → Maybe Val
get lens val = asVal $ getFromVal lens val
  where
    asVal ∷ HS.HashSet Val → Maybe Val
    asVal set 
      | HS.null set       = Nothing
      | HS.size set == 1  = Just $ L.head $ HS.toList set
      | otherwise         = Just $ Val_Set $ Set set



getFromVal ∷ Lens → Val → HS.HashSet Val
getFromVal (Lens_Set  setLens ) (Val_Set  set ) = getFromSet   setLens  set
getFromVal (Lens_Arr  arrLens ) (Val_Arr  arr ) = getFromArray arrLens  arr
getFromVal Lens_This             val            = HS.singleton val




getFromSet ∷ SetLens → Set → HS.HashSet Val
getFromSet (AllSuchThat ty lens) =
  let unionLenses accSet nextElem =
        if isType ty nextElem
          then accSet `HS.union` getFromVal lens nextElem
          else HS.empty
  in  HS.foldl' unionLenses HS.empty . _getSet
getFromSet (AnySuchThat ty lens) =
      _getSet
  >>> HS.toList
  >>> L.filter (isType ty)
  >>> listToMaybe
  >>> (\x → case x of
              Just elem → getFromVal lens elem
              Nothing   → HS.empty
      ) 
               


getFromArray ∷ ArrayLens → Array → HS.HashSet Val
getFromArray (AtIndices idxList) array =
  let unionLenses accSet (num, lens) =
        let mElem = asInt num >>= at array
        in  case mElem of
              Just elem → accSet `HS.union` getFromVal lens elem
              Nothing   → HS.empty
  in  L.foldl' unionLenses HS.empty idxList
getFromArray (EachIndex lens) (Arr arr) =
  F.foldl' (flip $ HS.union . getFromVal lens) HS.empty arr



