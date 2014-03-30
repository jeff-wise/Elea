

module Elea.Lang.Term.Value
  ( -- * Values
    Value (..)
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
  | Val_Type    Type        -- | Types are values
  deriving (Eq, Generic)


data Record = Rec (HMS.HashMap Text Value)
  deriving (Eq, Generic)


data Array = Arr (Seq.Seq Value)
  deriving (Eq, Generic)


data Set = Set (HS.HashSet Value)
  deriving (Eq, Generic)


data Text = Text T.Text
  deriving (Eq, Generic)


data Number = 
    Z Int 
  | R Double
  deriving (Generic)


data DateTime = DateTime Day TimeOfDay
  deriving (Eq, Generic)




---------------------------------------------------------------------
-- 2 Hashable Value
---------------------------------------------------------------------

instance Hashable Value
instance Hashable Record
instance Hashable Array
instance Hashable Set
instance Hashable Text
instance Hashable Number
instance Hashable DateTime




---------------------------------------------------------------------
-- 3 Show Values
---------------------------------------------------------------------

instance Show Value where
  show (Val_Rec  rec ) = show rec
  show (Val_Arr  arr ) = show arr
  show (Val_Set  set ) = show set
  show (Val_Text text) = show text
  show (Val_Num  num ) = show num
  show (Val_Dtm  dtm ) = show dtm
  show (Val_Type typ ) = show typ


instance Show Record where
  show (Rec rec) = show rec


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




---------------------------------------------------------------------
-- 4 Numeric  Classes
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
-- 5 Utility Functions
---------------------------------------------------------------------


-- 5.1 Constructors
---------------------------------------------------------------------

dict ∷ [(T.Text, Val)] → Val
dict entries = Val_Dict . Dict . HMS.fromList

text ∷ T.Text → Val
text = Val_Text . Text

int ∷ Int → Val
int = Val_Number . Z

real ∷ Double → Val
real = Val_Number . R



-- 5.2 Array Functions
---------------------------------------------------------------------

-- | Return a value at an index in an 'Array'
at ∷ Array → Int → Maybe Val
at (Arr arr) i 
  | i >= 0 =  if i < Seq.length arr
                then Just $ Seq.index arr i
                else Nothing
  | i < 0  =  if abs i <= Seq.length arr
                then Just $ Seq.index arr (Seq.length arr + i)
                else Nothing
  | otherwise = Nothing



-- 5.3 Number Functions
---------------------------------------------------------------------

-- | Check if a 'Number' is an Integer, and if so, return it
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



