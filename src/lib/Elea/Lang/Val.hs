

module Elea.Lang.Val (
    at
  , showSym  
  , doubleToInt
  , numEven, numOdd
  , isInteger
  , eval
  ) where



import Elea.Prelude
import Elea.Lang.Types



import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T




at ∷ Array → Int → Maybe Val
at (Arr arr) i 
  | i >= 0 =  if i < Seq.length arr
                then Just $ Seq.index arr i
                else Nothing
  | i < 0  =  if abs i <= Seq.length arr
                then Just $ Seq.index arr (Seq.length arr + i)
                else Nothing
  | otherwise = Nothing


-- Type constructors/functions

-- If nothing is returned, should throw exception?
-- Shouldn't be able to have programs which show
-- symbols that don't exist. Symbols are only 
-- kind of 'custom' atomic datatype.
-- Use symbols for authentication/security?
showSym ∷ Int → System → Maybe T.Text
showSym symId system = HMS.lookup symId (system ^. symTable.tblMap)



-- Returns Integer version of Double
-- if no information is lost in the conversion
doubleToInt ∷ Double → Maybe Int
doubleToInt double = 
  let i = round double
  in  if double == fromIntegral i
        then Just i
        else Nothing





numEven ∷ Number → Bool
numEven (Z i) = even i
numEven (R d) = maybe False even $ doubleToInt d
   

numOdd ∷ Number → Bool
numOdd (Z i) = odd i
numOdd (R d) = maybe False odd $ doubleToInt d


isInteger ∷ Number → Bool
isInteger (Z _) = True
isInteger (R d) = case doubleToInt d of
                    Just _  → True
                    Nothing → False





eval ∷ Env → Val → Val
eval env val = eval' val
  where
    eval' ∷ Val → Val
    eval' (Val_Pair     (Pair  a b)) = Val_Pair $ Pair (eval' a) (eval' b)
    eval' (Val_Set      (Set   set)) = Val_Set $ Set $ HS.map eval' set
    eval' (Val_Arr      (Arr arr)) = Val_Arr $ Arr $ fmap eval' arr
    eval' (Val_Var  var@(Var name )) = case HMS.lookup name env of
                                        Just x  → x
                                        Nothing → Val_Var $ var
    eval' leafVal                   = leafVal

