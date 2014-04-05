

module Elea.Lang.Term.Transformer where


import Elea.Prelude

import Elea.Lang.Term.Value


import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T



-----------------------------------------------------------
--------------------- TRANSFORMER -------------------------
-----------------------------------------------------------

data Transformer =
    Tr_Template Template
  | Tr_Equation Equation
  | Tr_Query    Query




----------------------- TEMPLATE --------------------------

data Template = Var T_Val


data Var a = 
    -- | Declaration
    D a
    -- | Binding (to a parameter in context)
  | B T.Text


data T_Val = 
    T_Val_Rec     T_Record
  | T_Val_Arr     T_Array
  | T_Val_Set     T_Set
  | T_Val_Text    T_Text
  | T_Val_Num     T_Number
  | T_Val_Null



data T_Record = T_Rec
  (HMS.HashMap (Var T.Text) (Var T_Val))


data T_Array = T_Arr (Seq.Seq (Var T_Val))


data T_Set = T_Set (HS.HashSet (Var T_Val))


newtype T_Text = T_Text T.Text


data T_Number = 
    T_Z Int 
  | T_R Double




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



