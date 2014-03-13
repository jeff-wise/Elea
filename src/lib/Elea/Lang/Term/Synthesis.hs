

module Elea.Lang.Term.Synthesis where




---------------------------------------------------------------------
-- 1 Synthesis
---------------------------------------------------------------------


-- 1.1 Aux Types
---------------------------------------------------------------------

type Param = Int
type Root = Int

-- Adjacency List representation, since functional
-- representations look difficult...
-- Vertices are just arbitrary integer identifiers
type SynthesisGraph = HMS.HashMap Int Application



-- 1.2 Synthesis
---------------------------------------------------------------------

-- | Synthesis is an 'Application' graph
data Synthesis = Synthesis Root SynthesisGraph



-- 1.3 Lambda Terms
---------------------------------------------------------------------
-- $lambda_terms
-- The naming scheme for the 'Synthesis' components is similar to the
-- lambda calculus components in order to emphasize the combinatorial
-- nature of synthesis.


-- | Represents an application of some defined function
-- to a list of parameters, which point to other applications
-- in the synthesis graph
data Application = Application Abstraction [Param]



-- | Abstraction
-- An abstraction is a pure function, of a specific purpose.
-- Abstractions computer certain types in ways specific to
-- that type, to enforce using readable and efficient data models
-- as well as modular, composable computations.
data Abstraction =
    Abs_Template  Template
  | Abs_Equation  Equation
  | Abs_TextProc  TextProcessor
  | Abs_Map       AbsMap
  | Abs_Query     Query
  | Abs_Rel       RelQuery
  | Abs_Trigger

  


---------------------------------------------------------------------
-- 1.3 Utility Function
---------------------------------------------------------------------

synthesis ∷ [(Int, Application)] → Synthesis
synthesis apps = Synthesis . HMS.fromList



valConst value = Synthesis $
  HMS.fromList [ (1, Application (Abs_Value value) []) ]


tyConst ty = Synthesis $
  HMS.fromList [ (1, Application (Abs_Type ty) []) ]


lensConst lens = Synthesis $
  HMS.fromList [ Application (Abs_Lens lens) [] ]




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
  | Ref Param



data T_Value = 
    T_Val_Dict    T_Dict
  | T_Val_Arr     T_Array
  | T_Val_Set     T_Set
  | T_Val_Text    T_Text
  | T_Val_Num     T_Number
  | T_Val_Dtm     T_DateTime
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




---------------------------------------------------------------------
-- 5.0 Query
---------------------------------------------------------------------

data Query = Query
  { _qryFrom    ∷ URITy
  , _qryWhere   ∷ Type
  , _qrySelect  ∷ Lens
  }




---------------------------------------------------------------------
-- 6.0 Random
---------------------------------------------------------------------

data Random =
    RandomNumber Type
  | MapRandom Param Abstraction


