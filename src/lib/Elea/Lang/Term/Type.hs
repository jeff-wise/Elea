

module Elea.Lang.Term.Type
  ( -- * Types
    Type (..)
  , RecordType (..), ArrayType (..)
  , AndType (..), OrType (..)
  , TextType (..), NumberType (..)
  ) where
 


import Elea.Prelude

import Elea.Lang.Term.Value


import Data.Foldable as F (foldr1)
import Data.List ((++))
import Data.Hashable
import qualified Data.Text as T

import GHC.Generics (Generic)




------------------------- ELEA TYPES -----------------------


data Type = 
    Ty_Rec    RecordType
  | Ty_Arr    ArrayType
  | Ty_And    AndType
  | Ty_Or     OrType
  | Ty_Txt    TextType
  | Ty_Num    NumberType
  -- | The Top type, T <: Ty_Top
  | Ty_Any 
  -- | The Bottom type, Ty_Bot <: T
--  | Ty_Bot 
  deriving (Eq, Generic)


data RecordType =
    HasEntry  T.Text Type
  | WithSize Int
  | AnyRecord
  deriving (Eq, Generic)


data ArrayType = 
    -- | For now, indices must be >=0
    WithIndex Int Type 
  | WithArrLen Int
  | AnyArray
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
  | Range         Number Number  -- | Inclusive
  -- | This currently only describes numbers which have a 
  -- 'Z' representation, e.g. '3.0' is not an integer
  | AnyNumber
  deriving (Eq, Generic)




-------------------------- HASHABLE ------------------------

instance Hashable Type
instance Hashable RecordType
instance Hashable ArrayType
instance Hashable OrType
instance Hashable AndType
instance Hashable TextType
instance Hashable NumberType




---------------------------- SHOW --------------------------

instance Show Type where
  show (Ty_Rec  recTy)  = show recTy
  show (Ty_Arr  arrTy)  = show arrTy
  show (Ty_And  andTy)  = show andTy
  show (Ty_Or   orTy )  = show orTy
  show (Ty_Txt  txtTy)  = show txtTy
  show (Ty_Num  numTy)  = show numTy
  show Ty_Any           = "Any"
 -- show Ty_Bot           = "Bottom"


instance Show RecordType where
  show (HasEntry label entryTy) =
    "HasEntry at " ++ (T.unpack label) ++ "\n of " ++ show entryTy
  show (WithSize size)          = "WithSize " ++ show size
  show AnyRecord                = "Record"


instance Show ArrayType where
  show (WithIndex idx idxTy)  =
    "Index at " ++ show idx ++ "\n of " ++ show idxTy
  show (WithArrLen len     )  = "Array with length " ++ show len
  show AnyArray               = "Array"


instance Show OrType where
  show (OrType tyList) =
    let conTypeStrs ty1 ty2 = ty1 ++ "\nOR " ++ ty2
    in  F.foldr1 conTypeStrs $ fmap show tyList


instance Show AndType where
  show (AndType tyList) =
    let conTypeStrs ty1 ty2 = ty1 ++ "\nOR " ++ ty2
    in  F.foldr1 conTypeStrs $ fmap show tyList


instance Show TextType where
  show (IsText      text) = "Text: " ++ show text
  show (WithTextLen len ) = "Text with length " ++ show len
  show AnyText            = "Text"


instance Show NumberType where
  show (IsNumber    num) = "Is " ++ show num
  show (GreaterThan lb ) = "Greater Than " ++ show lb
  show (LessThan    ub ) = "Lesser Than " ++ show ub
  show (Range    x  y  ) = 
    "[" ++ show x ++ ", " ++ show y ++ "]"
  show AnyNumber         = "Number"


