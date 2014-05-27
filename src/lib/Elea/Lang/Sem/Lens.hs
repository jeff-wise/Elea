

module Elea.Lang.Sem.Lens where


import Elea.Prelude

import Elea.Lang.Term.Lens
import Elea.Lang.Term.Value


import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq




lens ∷ LensMethod → Lens → Value → Maybe Value 
lens method = go
  where
    go (Lens_Rec recLens) (Val_Rec rec) = lensRecord method recLens rec
    go (Lens_Arr arrLens) (Val_Arr arr) = lensArray  method arrLens arr
    go Lens_This          value         = case method of
                                            Get     → Just value
                                            Put val → Just val
    go _                  _             = Nothing




lensRecord ∷ LensMethod → RecordLens → Record → Maybe Value
lensRecord method = go
  where
    go (AtLabel label entryLens) (Rec hm) = do
      recValue ← HMS.lookup label hm
      case method of 
        Get     → lens method entryLens recValue
        Put val → do
          modValue ← lens method entryLens recValue
          return $ Rec $ HMS.insert label modValue hm




lensArray ∷ LensMethod → ArrayLens → Array → Maybe Value
lensArray method = go
  where
    go (AtIndex i idxLens) (Arr arr) = do
      guard $ (i >= 0) && (i < Seq.length arr)
      let idxVal = Seq.index arr i
      case method of
        Get   → lens method idxLens idxVal 
        Put _ → do 
          idxVal' ← lens method idxLens idxVal
          return $ Arr $ Seq.update i idxVal' arr




