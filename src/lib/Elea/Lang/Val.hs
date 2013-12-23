

module Elea.Lang.Val ( 
    Val (..)
  , Set (..)
  , Pair (..)
  , Array (..)
  , Text (..)
  , Number (..)
  , Symbol (..)
  , DateTime (..)
  , Variable (..)
  , Error (..) 
  , AppError (..), ParamError (..)
  , Env
  , at
  , eval
  ) where



import Elea.Prelude
import Elea.Lang.Types

import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set




at ∷ Array → Int → Maybe Val
at (Array arr) i 
  | i >= 0 =  if i < Seq.length arr
                then Just $ Seq.index arr i
                else Nothing
  | i < 0  =  if abs i <= Seq.length arr
                then Just $ Seq.index arr (Seq.length arr + i)
                else Nothing




eval ∷ Env → Val → Val
eval env val = eval' val
  where
    eval' ∷ Val → Val
    eval' (Val_Pair     (Pair  a b)) = Val_Pair $ Pair (eval' a) (eval' b)
    eval' (Val_Set      (Set   set)) = Val_Set $ Set $ Set.map eval' set
    eval' (Val_Arr      (Array arr)) = Val_Arr $ Array $ fmap eval' arr
    eval' (Val_Var  var@(Var name )) = case HMS.lookup name env of
                                        Just x  → x
                                        Nothing → Val_Var $ var
    eval' leafVal                   = leafVal

