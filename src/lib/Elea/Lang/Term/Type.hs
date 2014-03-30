

module Elea.Lang.Term.Type
  ( -- * Types
  , Type (..)
  , RecTy (..), ArrayTy (..), SetTy (..)
  , AndTy (..), OrTy (..)
  , TextTy (..), NumberTy (..), DateTimeTy (..)
  ) where
 


import Elea.Prelude

import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import GHC.Generics (Generic)



---------------------------------------------------------------------
-- 2.0 Types
---------------------------------------------------------------------

data Type = 
    Ty_Dict   DictTy
  | Ty_Set    SetTy
  | Ty_Arr    ArrayTy
  | Ty_And    AndTy
  | Ty_Or     OrTy
  | Ty_Text   TextTy
  | Ty_Num    NumberTy
  | Ty_Dtm    DateTimeTy
  | Ty_URI    URITy
  | Ty_None 
  | Ty_Any 
  deriving (Eq, Generic)


data DictTy =
    HasEntry T.Text Type
  | DictOfSize Int
  | AnyDict
  deriving (Eq, Generic)


data ArrayTy = 
    WithIndex     Number Type 
  | ArrOfSize     Number Type 
  | EachArrValue  Type
  | AnyArray
  deriving (Eq, Generic)


data SetTy = 
    WithElem      Type
  | SetOfSize   Number
  | AnySet
  deriving (Eq, Generic)


-- | Intersection Type
-- E.g., If x is *AndTy [T1, T2, T3]*, then x satifies
-- property T1, T2, and T3.
data AndTy = AndTy [Type]
  deriving (Eq, Generic)


data OrTy = OrTy [Type]
  deriving (Eq, Generic)


data TextTy = 
    WithTextLen  Int
  | IsText       T.Text
  | OneOfText    (HS.HashSet T.Text)
  | AnyText
  deriving (Eq, Generic)


data NumberTy = 
    IsNumber      Number
  | GreaterThan   Number
  | LessThan      Number
  | InRange       Number Number  -- | Inclusive
  | Even
  | Odd
  | Integer
  | NonNegative
  | AnyNumber
  deriving (Eq, Generic)


data DateTimeTy = 
    IsDateTime DateTime
  | AnyDateTime
  deriving (Eq, Generic)


data URITy = URITy


---------------------------------------------------------------------
-- 2.1 Hashable Types
---------------------------------------------------------------------

instance Hashable Type
instance Hashable DictTy
instance Hashable ArrayTy
instance Hashable SetTy
instance Hashable OrTy
instance Hashable AndTy
instance Hashable TextTy
instance Hashable NumberTy
instance Hashable DateTimeTy




---------------------------------------------------------------------
-- 2.2 Showable Types
---------------------------------------------------------------------

instance Show Type where
  show (Ty_Dict dictTy) = show dictTy
  show (Ty_Set  setTy ) = show setTy
  show (Ty_Arr  arrTy ) = show arrTy
  show (Ty_And  andTy ) = show andTy
  show (Ty_Or   orTy  ) = show orTy
  show (Ty_Text textTy) = show textTy
  show (Ty_Num  numTy ) = show numTy
  show (Ty_Dtm  dtmTy ) = show dtmTy
  show  Ty_Any          = "Any"


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


instance Show DateTimeTy where
  show (IsDateTime dtm) = "Is " ++ show dtm
  show AnyDateTime      = "DateTime"




---------------------------------------------------------------------
-- 2.3 Utility Functions
---------------------------------------------------------------------

isDict ∷ Type
isDict entries = 
  let entryTys = L.map (\(key, ty) → HasEntry key ty) entries
      sizeTy = DictOfSize (L.length entries)
  in  AndTy (sizeTy : entryTys)


isText ∷ Type
isText = Ty_Text . IsText



