


module Elea.Lang.Term.Fun.Template where



---------------------------------------------------------------------
-- 3.0 Value Template
---------------------------------------------------------------------


-- | Template
-- A Template type is either a reference to a value of
-- some type or a declaration of that value
-- Abstracted normal values over with this type function.
-- and built new datatype around it. Mapping to values
-- is resovling this type function.
data Template a = 
    Dec a
  | Var T.Text



data T_Value = 
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


---------------------------------------------------------------------
-- 3.1 Hashable Value Template
---------------------------------------------------------------------

instance Hashable T_Set
instance Hashable T_Array
instance Hashable T_Dict
instance Hashable T_Text
instance Hashable T_Number
instance Hashable T_DateTime




---------------------------------------------------------------------
-- 4.0 Type Template
---------------------------------------------------------------------

data T_Type = 
    T_Ty_Dict   T_DictTy
  | T_Ty_Set    T_SetTy
  | T_Ty_Arr    T_ArrayTy
  | T_Ty_And    T_AndTy
  | T_Ty_Or     T_OrTy
  | T_Ty_Text   T_TextTy
  | T_Ty_Num    T_NumberTy
  | T_Ty_Dtm    T_DateTimeTy
  | T_Ty_Any 


data T_DictTy =
    T_HasEntry (Template T_Text) (Template T_Type)
  | T_DictOfSize (Template T_Num)


data T_SetTy = 
    T_WithElem      (Template T_Type)
  | T_SetWithSize   (Template T_Number)
  | T_AnySet


data T_ArrayTy = 
    T_IsArray    (Seq.Seq (Template T_Type))
  | T_WithIndex  (Template T_Number) (Template T_Type)
  | T_AnyArray


data T_AndTy = T_AndTy [(Template T_Type)]


data T_OrTy = T_OrTy [(Template T_Type)]


data T_TextTy = 
    T_WithTextLen  Int
  | T_IsText       T.Text
  | T_OneOfText    (Template (HS.HashSet T.Text))
  | T_AnyText


data T_NumberTy = 
    T_IsNumber      (Template T_Number)
  | T_GreaterThan   (Template T_Number)
  | T_LessThan      (Template T_Number)
  | T_InRange       (Template T_Number) (Template T_Number)
  | T_Even
  | T_Odd
  | T_Integer
  | T_NonNegative
  | T_AnyNumber


data T_DateTimeTy = 
    T_IsDateTime (Template T_DateTime)
  | T_AnyDateTime




