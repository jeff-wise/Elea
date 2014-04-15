

module Elea.Lang.Sem.Transform.Template where



import Elea.Prelude

import Elea.Lang.Sem.Types

import Elea.Lang.Term.Transformer
import Elea.Lang.Term.Value


import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HMS



template ∷ ParamMap → Template → Value
template paramMap = eval
  
  where

    eval (T_Rec (RecT rec)) = Val_Rec $ Rec $ fmap eval rec
    eval (T_Arr (ArrT arr)) = Val_Arr $ Arr $ fmap eval arr
    eval (T_Txt (TxtT txt)) = Val_Txt txt
    eval (T_Num (NumT num)) = Val_Num num
    eval (T_Var varName   ) = fromJust $ HMS.lookup varName paramMap





