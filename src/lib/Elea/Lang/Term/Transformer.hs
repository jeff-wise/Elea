

module Elea.Lang.Term.Transformer where


data Transformer =
    Tr_Equation Equation
  | Tr_TextProc TextProc
  | Tr_Template Template



----------------------- TEMPLATE --------------------------

data ValueTemplate a = 
    Dec a
  | Var T.Text

-- use network reference? if static?

data  = 
    T_Val_Dict    T_Dict
  | T_Val_Arr     T_Array
  | T_Val_Set     T_Set
  | T_Val_Text    T_Text
  | T_Val_Num     T_Number
  | T_Val_Dtm     T_DateTime
  | T_Val_Type    T_Type
  | T_Val_Null
  deriving (Generic)



data T_Dict = T_Dict
  (HMS.HashMap (Template T_Text) (Template T_Value))
  deriving (Generic)


data T_Array = T_Arr (Seq.Seq (Template T_Value))
  deriving (Generic)


data T_Set = T_Set (HS.HashSet (Template T_Value))
  deriving (Generic)


data T_Text = T_Text T.Text
  deriving (Generic)


data T_Number = 
    T_Z Int 
  | T_R Double
  deriving (Generic)


data T_DateTime = T_DateTime Day TimeOfDay
  deriving (Generic)





----------------------- EQUATION --------------------------

-- for now
-- use GADT
-- allow imaginary nums?
-- eventually allow full math equations?
-- keep simple for now

data Equation =
    Num Number 
  | Add Equation Equation
  | Sub Equation Equation
  | Mul Equation Equation
  | Div Equation Equation
  | Exp Equation Equation
  | Sqrt Equation





----------------------- TEXTPROC --------------------------



