

module Elea.Lang.Exp.Type
  ( -- * Types
    Type (..)
  , RecordType (..), ArrayType (..), SetType (..)
  , AndType (..), OrType (..)
  , TextType (..), NumberType (..)
  ) where
 


import Elea.Prelude

import Elea.Lang.Exp.Value


import Data.Hashable
import qualified Data.Text as T

import GHC.Generics (Generic)




------------------------- ELEA TYPES -----------------------


data Type = 
    Ty_Rec    RecordType
  | Ty_Set    SetType
  | Ty_Arr    ArrayType
  | Ty_And    AndType
  | Ty_Or     OrType
  | Ty_Text   TextType
  | Ty_Num    NumberType
  -- | The Top type, T <: Ty_Top
  | Ty_Top 
  -- | The Bottom type, Ty_Bot <: T
  | Ty_Bot 
  deriving (Eq, Generic)


data RecordType =
    HasEntry  T.Text Type
  | RecOfSize Int
  | AnyRecord
  deriving (Eq, Generic)


data ArrayType = 
    -- | For now, indices must be >=0
    WithIndex Int Type 
  | ArrOfSize Int
  | AnyArray
  deriving (Eq, Generic)


data SetType = 
    WithElem    Type
  | SetOfSize   Int
  | AnySet
  deriving (Eq, Generic)


-- | Intersection Type
-- Example: If x is *AndTy [T1, T2, T3]*, then x satifies
-- property T1, T2, and T3.
data AndType = AndType [Type]
  deriving (Eq, Generic)


-- | Union Type
data OrType = OrType [Type]
  deriving (Eq, Generic)


data TextType = 
    WithTextLen  Int
  | IsText       Text
  | AnyText
  deriving (Eq, Generic)


data NumberType = 
    IsNumber      Number
  | GreaterThan   Number
  | LessThan      Number
  | InRange       Number Number  -- | Inclusive
  -- | This currently only describes numbers which have a 
  -- 'Z' representation, e.g. '3.0' is not an integer
  | Integer
  | NonNegative
  | AnyNumber
  deriving (Eq, Generic)




-------------------------- HASHABLE ------------------------

instance Hashable Type
instance Hashable RecordType
instance Hashable ArrayType
instance Hashable SetType
instance Hashable OrType
instance Hashable AndType
instance Hashable TextType
instance Hashable NumberType




---------------------------- SHOW --------------------------
{-
instance Show Type where
  show (Ty_Rec  recTy)  = show recTy
  show (Ty_Set  setTy ) = show setTy
  show (Ty_Arr  arrTy ) = show arrTy
  show (Ty_And  andTy ) = show andTy
  show (Ty_Or   orTy  ) = show orTy
  show (Ty_Text textTy) = show textTy
  show (Ty_Num  numTy ) = show numTy


instance Show RecordTy where
  show = ""


instance Show ArrayTy where
  show (IsArray arr)    = "Is " ++ show arr
  show (WithIndex i ty) = "At " ++ show i 
    ++ " type of " ++ show ty
  show AnyArray         = "Array"


instance Show SetTy where
  show (WithElem ty) = 
    "WithElem of " ++ show ty
  show (SetWithSize size) = 
    "Set with size of " ++ show size
  show (IsSet set) = "Is " ++ show set
  show  AnySet       = "Set"


instance Show TextTy where
  show (WithTextLen len ) = "Length: " ++ show len
  show (IsText    text) = "Is " ++ show text
  show AnyText          = "Text"


instance Show OrTy where
  show (OrTy ty1 ty2) = 
    show ty1 ++ "\nOR\n" ++ show ty2


instance Show AndTy where
  show (AndTy ty1 ty2) = 
    show ty1 ++ "\nAND\n" ++ show ty2


instance Show NumberTy where
  show (IsNumber    num) = "Is " ++ show num
  show (GreaterThan lb ) = "Greater Than " ++ show lb
  show (LessThan    ub ) = "Lesser Than " ++ show ub
  show (InRange  x y   ) = 
    "[" ++ show x ++ ", " ++ show y ++ "]"
  show Even              = "Even"
  show Odd               = "Odd"
  show Integer           = "Integer"
  show NonNegative       = "NonNegative"
  show AnyNumber         = "Number"

-}

