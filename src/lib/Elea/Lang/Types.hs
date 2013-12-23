

---------------------------------------------------------------------
-- | 
-- Module with all of the main types.
--
-- Necessary to avoid circular imports
---------------------------------------------------------------------
module Elea.Lang.Types where


import Elea.Prelude

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import GHC.Generics (Generic)



---------------------------------------------------------------------
-- Elea Values
---------------------------------------------------------------------

data Val = 
    Val_Set     Set
  | Val_Pair    Pair
  | Val_Arr     Array
  | Val_Text    Text
  | Val_Num     Number
  | Val_Sym     Symbol
  | Val_Dtm     DateTime
  | Val_Var     Variable
  | Val_Err     Error
  deriving (Eq, Generic)

instance Hashable Val


data Set = Set
  { _getSet ∷ HashSet Val } 
  deriving (Eq, Generic)

instance Hashable Set


data Pair = Pair
  { _first  ∷  Val
  , _second ∷  Val
  } deriving (Eq, Generic)

instance Hashable Pair


data Array = Array 
  { _getArr ∷  Seq.Seq Val }
  deriving (Eq, Generic)

instance Hashable Array


data Text = Text 
  { _getText ∷ T.Text }
  deriving (Eq, Generic)

instance Hashable Text


-- Internally either int or float
-- Automatically casted when necessary.
data Number = 
    Z Int 
  | R Double
  deriving (Generic)


instance Eq Number where
  (==) (Z x) (Z y) = x == y
  (==) (Z i) (R d) = fromIntegral i == d
  (==) (R x) (R y) = x == y
  (==) (R d) (Z i) = d == fromIntegral i


instance Ord Number where
  compare (Z x) (Z y) = compare x y
  compare (R x) (R y) = compare x y
  compare (Z i) (R d) = compare (fromIntegral i) d
  compare (R d) (Z i) = compare d (fromIntegral i)

instance Hashable Number


data Symbol = Symbol 
  { _getSym ∷  BS.ByteString }
  deriving (Eq, Generic)

instance Hashable Symbol


data DateTime = DateTime 
  { _date ∷  Day
  , _time ∷  TimeOfDay
  } deriving (Eq, Generic)

instance Hashable DateTime


data Variable = Var
  { _varName  ∷  Val }
  deriving (Eq, Generic)

instance Hashable Variable


-- Error Values

data Error = Err_App AppError
  deriving (Eq, Generic)

instance Hashable Error


data AppError = 
    AppParamError [(Either ParamError Val)]
  | AppFunNotFound
  | ExecError
  deriving (Eq, Generic)

instance Hashable AppError


data ParamError = 
    ParamLensNotFound 
  | UnexpectedParamType Type Val
  | RefColDoesNotExist
  deriving (Eq, Generic)

instance Hashable ParamError




---------------------------------------------------------------------
-- Elea Types
---------------------------------------------------------------------

data Type = 
    Ty_Set    SetTy
  | Ty_Pair   PairTy
  | Ty_Arr    ArrayTy
  | Ty_And    AndTy
  | Ty_Or     OrTy
  | Ty_Text   TextTy
  | Ty_Num    NumberTy
  | Ty_Sym    SymbolTy
  | Ty_Dtm    DateTimeTy
  | Ty_Any 
  deriving (Eq, Generic)

instance Hashable Type


data SetTy = 
    WithElem    Type
  | SetWithSize Int
  | EqToSet     (HashSet Val)
  | AnySet
  deriving (Eq, Generic)

instance Hashable SetTy


data PairTy = 
    Pair_EQ Type Type
  | First   Type
  | Second  Type
  | AnyPair
  deriving (Eq, Generic)

instance Hashable PairTy


data ArrayTy = 
    EqToArr   (Seq.Seq Val)
  | WithIndex Int Type 
  | AnyArray
  deriving (Eq, Generic)

instance Hashable ArrayTy


data AndTy = 
  AndTy Type Type
  deriving (Eq, Generic)
 
instance Hashable AndTy


data OrTy = 
  OrTy  Type Type
  deriving (Eq, Generic)

instance Hashable OrTy


data TextTy = 
    OfTextLen  Int
  | IsText     T.Text
  | AnyText
  deriving (Eq, Generic)

instance Hashable TextTy


data NumberTy = 
    IsNumber      Number
  | GreaterThan   Number
  | LessThan      Number
  | InRange       Number Number  -- | Inclusive
  | Even
  | Odd
  | Natural
  | NonNegative
  | AnyNumber
  deriving (Eq, Generic)

instance Hashable NumberTy


data SymbolTy = 
    IsSymbol BS.ByteString
  | AnySymbol
  deriving (Eq, Generic)

instance Hashable SymbolTy


data DateTimeTy = 
    IsDateTime DateTime
  | AnyDateTime
  deriving (Eq, Generic)

instance Hashable DateTimeTy


---------------------------------------------------------------------
-- System
---------------------------------------------------------------------

type Env = HMS.HashMap Val Val


data System = System 
  { _sysVal     ∷  Val
  , _sysEnv     ∷  Env
  }


data Particle = Particle
  { _partId   ∷  Val
  , _partVal  ∷  Val
  }




---------------------------------------------------------------------
-- Value Lenses
---------------------------------------------------------------------

data Lens = 
    Lens_Set    SetLens
  | Lens_Pair   PairLens
  | Lens_Arr    ArrayLens
  | Lens_This


data SetLens = 
    AnySuchThat Type Lens
  | FstSuchThat Type Lens


data PairLens = 
    AtFirst Lens
  | AtSecond Lens
  | AtBoth Lens Lens


data ArrayLens = AtIndex Int Lens




---------------------------------------------------------------------
-- Value Constructors
---------------------------------------------------------------------

data Constructor = 
    Con_Val Val



---------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------

type FunDict = HMS.HashMap T.Text FunDef

data FunDef = FunDef
  { _funParamTys  ∷  [Type]
  , _funReturnTy  ∷  Type
  , _funExec      ∷  [Val] → Val
  }


