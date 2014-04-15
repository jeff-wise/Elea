

module Elea.Lang.Term.Transformer where



import Elea.Lang.Term.Value


import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import qualified Data.Text as T



-----------------------------------------------------------
--------------------- TRANSFORMER -------------------------
-----------------------------------------------------------

data Transformer =
    Tr_Template Template
--  | Tr_Equation Equation
 -- | Tr_Query    Query




----------------------- TEMPLATE --------------------------

type Template = T_Val


data T_Val = 
    T_Rec   RecordT
  | T_Arr   ArrayT
  | T_Txt   TextT
  | T_Num   NumberT
  | T_Var   T.Text


data RecordT = RecT
  (HMS.HashMap T.Text T_Val)


data ArrayT = ArrT (Seq.Seq T_Val)


newtype TextT = TxtT Text


newtype NumberT = NumT Number


{-

----------------------- EQUATION --------------------------

data Equation =
    Num Number 
  | Add Equation Equation
  | Sub Equation Equation
  | Mul Equation Equation
  | Div Equation Equation
  | Exp Equation Equation
  | Sqrt Equation





------------------------- QUERY ----------------------------

data Query = Query SystemId Type


-}
