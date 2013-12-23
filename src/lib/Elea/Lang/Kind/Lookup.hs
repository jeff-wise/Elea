


module Elea.Lang.Kind.Lookup where


import Data.HashMap.Strict as HS


lookupTy ∷ Type → Kind → MatchMap
lookupTy (Ty_Pair pairTy) kind = lookupPairTy pairTy $ get pairKind kind
lookupTy (Ty_Set  setTy ) kind = lookupSetTy  setTy  $ get setKind  kind
lookupTy (Ty_Text textTy) kind = lookupTextty textTy $ get textKind kind


lookupPairTy ∷ PairTy → PairKind → MatchMap
lookupPairTy (Pair_EQ tyA tyB) tyPairs = 
  foldl' matchUnion HS.empty $ for tyPairs $ (\ndA ndB → 
    lookupTy tyA ndA `matchIntersect` lookupTy tyB ndB
  )
lookupPairTy (First   ty) fstTypes = 
    foldl' matchUnion HS.empty $ lookupTy ty
lookupPairTy (Second  ty) sndTypes = 
    foldl' matchUnion HS.empty $ lookupTy ty



lookupSetTy ∷ SetTy → SetKind → MatchMap
lookupSetTy (Elem   ty ) (SetKind elemKinds _ _) =
  foldl' matchUnion HS.empty $ 
    map (lookupTy ty) elemKinds
lookupSetTy (SetLen len) (SetKind _ lenKinds _ ) = 
  case lookup len lenMap of
    Just keys → matchMap keys
    Nothing   → HS.empty
lookupSetTy (SetEQ  set) (SetKind _ _ eqKinds  ) = 
  for eqSets $ (\(eqSet, keys) →
    if eqSet == set then keys else HS.empty
  )


lookupTextTy ∷ TextTy → TextKind → MatchMap
lookupTextTy (Text_Len len ) textKind =
  maybe HS.empty matchMap $ 
    lookup len $ get textLenKinds textKind
lookupTextTy (Text_EQ  text) textKind = 
  maybe HS.empty matchMap $
    lookup text $ get textEqKinds textKind


-- TODO and, or, subset/superset
-- insert



--  | Ty_And    AndTy
--  | Ty_Or     OrTy







-- Util
----------------------------------
matchUnion = HS.unionWith (+)
matchIntersect = HS.intersectWith (+)

matchMap ∷ Set MatchKey → MatchMap
matchMap matchKeys = HS.fromList [(key, 1) | key ← matchKeys]



