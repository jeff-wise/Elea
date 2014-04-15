

module Elea.Lang.Sem.Lens where


import Elea.Prelude

import Elea.Lang.Term.Lens
import Elea.Lang.Term.Value


import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq



get ∷ Lens → Value → Maybe Value 
get (Lens_Rec recLens) (Val_Rec rec) = getRecord recLens rec
get (Lens_Arr arrLens) (Val_Arr arr) = getArray  arrLens arr
get Lens_This          value         = Just value 
get _                  _             = Nothing



getRecord ∷ RecordLens → Record → Maybe Value
getRecord (AtLabel label lens) (Rec hm) =
  HMS.lookup label hm >>= get lens


getArray ∷ ArrayLens → Array → Maybe Value
getArray (AtIndex i lens) (Arr arr) =
  if i >=0
    then get lens $ Seq.index arr i
    else Nothing


