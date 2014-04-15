

module Elea.Lang.Term.Value
  ( -- * Values
    Value (..)
  , Record (..), Array (..)
  , Text (..), Number (..)
  ) where
 


import Elea.Prelude

import Data.Hashable
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import GHC.Generics (Generic)



---------------------------- TYPES -------------------------

data Value = 
    Val_Rec   Record
  | Val_Arr   Array
  | Val_Txt   Text
  | Val_Num   Number
  deriving (Eq, Generic)


data Record = Rec (HMS.HashMap T.Text Value)
  deriving (Eq, Generic)


data Array = Arr (Seq.Seq Value)
  deriving (Eq, Generic)


newtype Text = Txt
  { primText  ∷ T.Text }
  deriving (Eq, Generic)


data Number = 
    Z Int 
  | R Double
  deriving (Generic)


{- NOT IN USE
data DateTime = DateTime Day TimeOfDay
  deriving (Eq, Generic)
-}



---------------------------------------------------------------------
-- 2 Hashable Value
---------------------------------------------------------------------


-- Make these collections Hashable --
-------------------------------------

instance Hashable a ⇒ Hashable (Seq.Seq a) where
  hashWithSalt = hashUsing F.toList  

instance (Hashable a, Hashable b) ⇒ Hashable (HMS.HashMap a b) where
  hashWithSalt = hashUsing HMS.toList  



-- Use Generic instances ------------
-------------------------------------

instance Hashable Value
instance Hashable Record
instance Hashable Array
instance Hashable Text
instance Hashable Number




---------------------------------------------------------------------
-- 3 Show Values
---------------------------------------------------------------------

instance Show Value where
  show (Val_Rec  rec) = show rec
  show (Val_Arr  arr) = show arr
  show (Val_Txt  txt) = show txt
  show (Val_Num  num) = show num


instance Show Record where
  show (Rec rec) = show rec


instance Show Array where
  show (Arr arr) = show arr


instance Show Text where
  show (Txt text) = show text


instance Show Number where
  show (Z i) = show i
  show (R d) = show d




---------------------------------------------------------------------
-- 4 Numeric  Classes
---------------------------------------------------------------------

instance Eq Number where
  (==) (Z x) (Z y) = x == y
  (==) (Z i) (R d) = (fromIntegral i ∷ Double) == d
  (==) (R x) (R y) = x == y
  (==) (R d) (Z i) = d == (fromIntegral i ∷ Double)


instance Ord Number where
  compare (Z x) (Z y) = compare x y
  compare (R x) (R y) = compare x y
  compare (Z i) (R d) = compare (fromIntegral i :: Double) d
  compare (R d) (Z i) = compare d (fromIntegral i :: Double)


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
{-
mkRec ∷ [(T.Text, Value)] → Value
mkRec = Val_Rec . Rec . HMS.fromList

text ∷ T.Text → Value
text = Val_Text . Text

int ∷ Int → Value
int = Val_Num . Z

real ∷ Double → Value
real = Val_Num . R

-}

-- 5.2 Array Functions
---------------------------------------------------------------------
{-
-- | Return a value at an index in an 'Array'
at ∷ Array → Int → Maybe Value
at (Arr arr) i 
  | i >= 0 =  if i < Seq.length arr
                then Just $ Seq.index arr i
                else Nothing
  | i < 0  =  if abs i <= Seq.length arr
                then Just $ Seq.index arr (Seq.length arr + i)
                else Nothing
  | otherwise = Nothing

-}

-- 5.3 Number Functions
---------------------------------------------------------------------

-- | Check if a 'Number' is an Integer, and if so, return it
-- Rounding errors?
{-
asInt ∷ Number → Maybe Int
asInt (Z i) = Just i
asInt (R d) =
  let i = round d
  in  if d == (fromIntegral i)
        then Just i
        else Nothing
-}
{-
numEven ∷ Number → Bool
numEven = maybe False even . asInt
   

numOdd ∷ Number → Bool
numOdd = maybe False odd . asInt


isInteger ∷ Number → Bool
isInteger = maybe False (const True) . asInt 


-}
