


module Elea.Lang.Fun.TypeTemplate where

{-
evalTy ∷ HMS.HashMap Val Val → Type → Type
evalTy env val = eval val
  where
    eval ∷ Type → Type
    eval (Ty_Set   (WithElem  ty       )) = 
    eval (Ty_Set   (IsSet     tySet    )) = 
    eval (Ty_Arr   (IsArray   tySeq    )) = 
    eval (Ty_Arr   (WithIndex idx   ty )) =
    eval (Ty_And   (AndTy     ty1   ty2)) = 
    eval (Ty_Or    (OrTy      ty1   ty2)) =
    eval (Ty_TyVar (TyVar     val      )) = case HMS.lookup 
    eval ty                             = ty

-}
