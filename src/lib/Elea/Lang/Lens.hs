


module Elea.Lang.Lens where


import Elea.Prelude
import Elea.Lang.Types
import Elea.Lang.Type (isType)
import Elea.Lang.Val (at)

import qualified Data.HashSet as Set
import qualified Data.List.Stream as L




get ∷ Lens → Val → Maybe Val
get (Lens_Set  setLens ) (Val_Set  set ) = getFromSet   setLens  set
get (Lens_Pair pairLens) (Val_Pair pair) = getFromPair  pairLens pair
get (Lens_Arr  arrLens ) (Val_Arr  arr ) = getFromArray arrLens  arr
get Lens_This             val            = Just val
get _                    _               = Nothing




getFromSet ∷ SetLens → Set → Maybe Val

getFromSet (AllSuchThat ty lens) =
      _getSet >>> Set.toList >>>            -- 1) filter set by type
      L.filter (isType ty) >>> return
  >=> (\elemsOfTy → if L.null elemsOfTy    -- 2) ensure set is non-empty
                       then Nothing
                       else Just elemsOfTy
      )
  >=> fmap (get lens)     >>>               -- 3) apply lens to each elem and
      catMaybes           >>>               --    return vals found as set
      (Just . Val_Set . Set . Set.fromList)

getFromSet (AnySuchThat ty lens) =
      _getSet >>> Set.toList >>> 
      L.filter (isType ty) >>> listToMaybe
  >=> get lens



getFromArray ∷ ArrayLens → Array → Maybe Val
getFromArray (AtIndex i lens) array = array `at` i >>= get lens
 

getFromPair ∷ PairLens → Pair → Maybe Val
getFromPair (AtFirst  lens) (Pair a _) = get lens a
getFromPair (AtSecond lens) (Pair _ b) = get lens b
getFromPair (AtBoth lensA lensB ) (Pair a b) = 
  fmap Val_Pair $ Pair <$> (get lensA a) <*> (get lensB b)




