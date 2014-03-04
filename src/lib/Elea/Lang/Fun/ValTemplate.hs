

-- | Value template functions
module Elea.Lang.Fun.ValTemplate where


import Elea.Prelude
import Elea.Lang.Fun.Types
import Elea.Lang.Exp.Types (Env)




evalValTemplate ∷ Env → Val → Val
evalValTemplate env val = eval val
  where
    eval ∷ Val → Val
    eval (Val_Set      (Set set )) = Val_Set $ Set $ HS.map eval set
    eval (Val_Arr      (Arr arr )) = Val_Arr $ Arr $ fmap eval arr
    eval (Val_Var  var@(Var name)) = case HMS.lookup name env of
                                        Just x  → x
                                        Nothing → Val_Var $ var
    eval leafVal                   = leafVal


