

-- | An efficient map of types to data items.
-- Just like in any other map implementation, we
-- compose the keys (types) in an efficient way
-- to reduce redundant information, and therefore
-- improve lookup time.
module Elea.Lang.Kind.Types where


import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as Set

import qualified Val as Val


type MatchKeys = Set.Set MatchKey

-- keep in mind, types are just value set constructors

data Kind = Kind
  { _pairKind   ∷  PairKind
  , _setKind    ∷  SetKind
  }


data PairKind = PairKind
  { _pairEqKinds   ∷  [(Kind, Kind)]
  , _pairFstKinds  ∷  [Kind]
  , _pairSndKinds  ∷  [Kind]
  }


data SetKind = SetKind
  { _elemKinds    ∷  [Kind]
  , _setLenKinds  ∷  HMS.HashMap Int MatchKeys
  , _setEqKinds   ∷  [(Set.Set Val, MatchKeys)]
  }


data TextKind = TextKind
  { _textLenKinds ∷  HMS.HashMap Int MatchKeys
  , _textEqKinds  ∷  HMS.HashMap Text MatchKeys
  }


data NumKind = NumKind
  { _numEqKinds   ∷  


data Type = 
  | Ty_Sym    SymTy
  | Ty_Dtm    DtmTy
  | Ty_Any
  | Ty_Void


data NumTy = 
    Num_Equal     Int
  | Num_GT        Int
  | Num_LT        Int
  | Num_InRange   Int Int  -- Inclusive
  | Num_Even
  | Num_Odd



-- Product type
data PairTy = 
    Pair_EQ Type Type
  | First   Type
  | Second  Type



data ArrTy = 
    Arr_EQ  Seq.Seq
  | IndexOf Int Type 




