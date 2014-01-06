

module Elea.Lang.Constructor where


import Elea.Lang.Types


-- Construct a value
--  | Con_Query Query
-- list comprehension




construct ∷ System → Constructor → Val
construct _ (Con_Val val) = val

{-
data Query = Query
  { _idCrit   ∷ Type
  , _valCrit  ∷ Type
  }


data RelQuery = RelQuery
  { _relId    ∷  RelationId
  , _valCrit  ∷  Type
  }

-}


