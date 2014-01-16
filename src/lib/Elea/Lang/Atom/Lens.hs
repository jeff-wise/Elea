


module Elea.Lang.Atom.Lens
  ( Lens (..), SetLens (..), ArrayLens (..)
  , get
  , getFromFst, getFromSnd, getFromBoth
  , atIndex
  ) where


import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Atom.Type
import Elea.Lang.Atom.Val (asInt, at)


import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.List.Stream as L




---------------------------------------------------------------------
-- Elea Value Lenses
---------------------------------------------------------------------

-- | Value Lens
data Lens = 
    Lens_Set    SetLens
  | Lens_Arr    ArrayLens
  | Lens_This


-- | Set Lens
data SetLens = 
    AllSuchThat Type Lens
  | AnySuchThat Type Lens


-- | Array Lens
data ArrayLens =
    AtIndices [(Number, Lens)]
  | EachIndex Lens


-- | Lens convenience constructors
getFromFst ∷ Lens → Lens
getFromFst lens = Lens_Arr $ AtIndices [(Z 0, lens)]


getFromSnd ∷ Lens → Lens
getFromSnd lens = Lens_Arr $ AtIndices [(Z 1, lens)]


getFromBoth ∷ Lens → Lens → Lens
getFromBoth lensA lensB = Lens_Arr $
    AtIndices [(Z 0, lensA), (Z 1, lensB)]


atIndex ∷ Number → Lens → Lens
atIndex idx lens = Lens_Arr $ AtIndices [(idx, lens)]



-- | Get a value referenced by a 'Lens'
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



