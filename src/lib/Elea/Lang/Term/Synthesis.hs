

module Elea.Lang.Term.Synthesis where



-- Abstractions

-- | Type Templates


data Template a = 
    Dec a
  | Var Int



data T_Value = 
    T_Val_Set     T_Set
  | T_Val_Dict    T_Dict
  | T_Val_Arr     T_Array
  | T_Val_Text    T_Text
  | T_Val_Num     T_Number
  | T_Val_Dtm     T_DateTime
  | T_Val_Null
  deriving (Generic)



data T_Set = T_Set (HS.HashSet (Template T_Value))
  deriving (Generic)


data T_Array = T_Arr (Seq.Seq (Template T_Value))
  deriving (Generic)


data T_Dict = T_Dict
  (HMS.HashMap (Template T_Text) (Template T_Value))
  deriving (Generic)


data T_Text = T_Text T.Text
  deriving (Generic)


data T_Number = 
    T_Z Int 
  | T_R Double
  deriving (Generic)


data T_DateTime = T_DateTime Day TimeOfDay
  deriving (Generic)


instance Hashable T_Set
instance Hashable T_Array
instance Hashable T_Dict
instance Hashable T_Text
instance Hashable T_Number
instance Hashable T_DateTime



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
  | T_IsDict [(Template T_Text, Template T_Type)]


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



-- Adjacency List representation, since functional
-- representations look difficult...
-- Vertices are just arbitrary integer identifiers
type SynthesisGraph = HMS.HashMap Int Application

-- | Synthesis is an 'Application' graph
data Synthesis = Synthesis SynthesisGraph


synthesis ∷ [(Int, Application)] → Synthesis
synthesis apps = Synthesis . HMS.fromList


synValueConst value = Synthesis $
  HMS.fromList [ Application (Abs_Value value) [] ]


synTypeConst typ = Synthesis $
  HMS.fromList [ Application (Abs_Type typ) [] ]


synLensConst lens = Synthesis $
  HMS.fromList [ Application (Abs_Lens lens) [] ]




-- Lambda calculus terms to emphasize
-- combinatorial nature of synthesis 

data Application = Application
  { _appAbs     ∷  Abstraction
  , _appParams  ∷ [Int]
  }


data Abstraction =
    Abs_Equation  Equation
  | Abs_TextProc  TextProcessor
  | Abs_Value     Value
  | Abs_Type      Type
  | Abs_Lens      Lens
  | Abs_SynValue  Synthesis
  | Abs_SynType   Synthesis
  | Abs_ValTemp   T_Val
  | Abs_TypeTemp  T_Type
 -- | Abs_LensTemp  TypeTemplate
  | Abs_Query     Query
  | Abs_Rel       RelQuery
  | Abs_CauseVal


data Query = Query
  { _qryFrom    ∷ SystemPath
  , _qryWhere   ∷ Type
  , _qrySelect  ∷ Lens
  }


