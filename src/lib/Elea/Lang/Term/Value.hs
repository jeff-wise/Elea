

module Elea.Lang.Term.Value
  ( -- * Values
    Val (..)
  , Set (..), Array (..), pair
  , Text (..), Number (..), Symbol (..)
  , DateTime (..), Variable (..)
  , Error (..)
  , SynError (..), AppError (..)
  , ParamError (..), UnivError (..)
    -- * Types
  , Type (..)
  , SetTy (..), ArrayTy (..)
  , pairTy, fstTy, sndTy
  , AndTy (..), OrTy (..)
  , TextTy (..), NumberTy (..), SymbolTy (..)
  , DateTimeTy (..), VariableTy (..), TyVariable (..)
    -- * Lens
  , Lens (..)
  , DictLens (..), ArrLens (..)
  ) where
 


import Elea.Prelude

import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import GHC.Generics (Generic)



---------------------------------------------------------------------
-- 1. Values
---------------------------------------------------------------------

data Value = 
    Val_Set     Set
  | Val_Dict    Dict
  | Val_Arr     Array
  | Val_Text    Text
  | Val_Num     Number
  | Val_Dtm     DateTime
  | Val_URI     URI
  | Val_Err     Error
  | Val_Type    Type
  | Val_Null
  deriving (Eq, Generic)


data Dict = Dict
  { _getDict ∷ (HMS.HashMap Text Value) }
  deriving (Eq, Generic)


data Array = Arr
  { _getArr ∷  Seq.Seq Value }
  deriving (Eq, Generic)


data Set = Set
  { _getSet ∷ HS.HashSet Value } 
  deriving (Eq, Generic)


data Text = Text 
  { _getText ∷ T.Text }
  deriving (Eq, Generic)


data Number = 
    Z Int 
  | R Double
  deriving (Generic)


data DateTime = DateTime 
  { _date ∷  Day
  , _time ∷  TimeOfDay
  } deriving (Eq, Generic)


data URI = URI
  deriving (Eq, Generic)


data Error =
    Err_Syn  Text SynError
  | Err_Univ UnivError
  | Err_Fun  FunError
  deriving (Eq, Generic)



data UnivError =
    -- | System not found at specified location
    CannotFindSystemLocation -- SystemLoc
    -- | New system is not permitted in the target system
  | SystemNotPermitted -- System System
    -- | System already contains similar system
  | SystemAlreadyExists
  deriving (Eq, Generic)




---------------------------------------------------------------------
-- 1.1 Hashable Value
---------------------------------------------------------------------

instance Hashable Value
instance Hashable Dict
instance Hashable Array
instance Hashable Set
instance Hashable Text
instance Hashable Number
instance Hashable DateTime
instance Hashable Error
instance Hashable UnivError




---------------------------------------------------------------------
-- 1.2 Show Value
---------------------------------------------------------------------

instance Show Value where
  show (Val_Set  set ) = show set
  show (Val_Arr  arr ) = show arr
  show (Val_Text text) = show text
  show (Val_Num  num ) = show num
  show (Val_Dtm  dtm ) = show dtm
  show (Val_Err  err ) = show err
  show (Val_Type typ ) = show typ
  show (Val_Null     ) = "Null"


instance Show Dict where
  show (Dict dict) = show dict


instance Show Array where
  show (Arr arr) = show arr


instance Show Set where
  show (Set hs) = show hs


instance Show Text where
  show (Text text) = show text


instance Show Number where
  show (Z i) = show i
  show (R d) = show d


instance Show DateTime where
  show (DateTime date time) =
    show date ++ " " ++ show time


instance Show Error where
  show (Err_Syn synName err) =
    "Error in Synthesis " ++ show synName
    ++ "\nError:\n" ++ show err


instance Show UnivError where
  show _ = "Universe Error"




---------------------------------------------------------------------
-- 1.3 Numeric  Classes
---------------------------------------------------------------------

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


instance Num Number where

  (+) (Z x) (Z y) = Z $ x + y
  (+) (Z x) (R y) = R $ fromIntegral x + y
  (+) (R x) (Z y) = R $ x + fromIntegral y
  (+) (R x) (R y) = R $ x + y

  (-) (Z x) (Z y) = Z $ x + y
  (-) (Z x) (R y) = R $ fromIntegral x + y
  (-) (R x) (Z y) = R $ x + fromIntegral y
  (-) (R x) (R y) = R $ x + y
  
  (*) (Z x) (Z y) = Z $ x + y
  (*) (Z x) (R y) = R $ fromIntegral x + y
  (*) (R x) (Z y) = R $ x + fromIntegral y
  (*) (R x) (R y) = R $ x + y

  negate (Z i) = Z $ negate i
  negate (R d) = R $ negate d

  abs (Z i) = Z $ abs i
  abs (R d) = R $ abs d

  signum (Z i) = Z $ signum i
  signum (R d) = R $ signum d

  fromInteger = Z . fromInteger




---------------------------------------------------------------------
-- 1.4 Utility Functions
---------------------------------------------------------------------

dict ∷ [(T.Text, Val)] → Val
dict entries = Val_Dict . Dict . HMS.fromList

text ∷ T.Text → Val
text = Val_Text . Text

int ∷ Int → Val
int = Val_Number . Z

real ∷ Double → Val
real = Val_Number . R



at ∷ Array → Int → Maybe Val
at (Arr arr) i 
  | i >= 0 =  if i < Seq.length arr
                then Just $ Seq.index arr i
                else Nothing
  | i < 0  =  if abs i <= Seq.length arr
                then Just $ Seq.index arr (Seq.length arr + i)
                else Nothing
  | otherwise = Nothing


-- | Casts a 'Double' as an 'Int', but only if
-- no information is lost in the conversion
-- TODO Removal Candidate
doubleToInt ∷ Double → Maybe Int
doubleToInt double = 
  let i = round double
  in  if double == fromIntegral i
        then Just i
        else Nothing


asInt ∷ Number → Maybe Int
asInt (Z i) = Just i
asInt (R d) =
  let i = round d
  in  if d == (fromIntegral i)
        then Just i
        else Nothing


numEven ∷ Number → Bool
numEven (Z i) = even i
numEven (R d) = maybe False even $ doubleToInt d
   

numOdd ∷ Number → Bool
numOdd (Z i) = odd i
numOdd (R d) = maybe False odd $ doubleToInt d


isInteger ∷ Number → Bool
isInteger = maybe False (const True) . asInt 




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




---------------------------------------------------------------------
-- 3.0 Lens
---------------------------------------------------------------------

data Lens = 
    Lens_Dict   DictLens
  | Lens_Arr    ArrLens
  | Lens_This


data DictLens =
    AtKey Text Lens
  | Entries [Text]


data ArrLens = AtIndex Int Lens



