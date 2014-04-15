

module Elea.Lang.Term.Lens where


import Elea.Prelude

import qualified Data.Text as T



------------------------- LENS -----------------------------


data Lens = 
    Lens_Rec   RecordLens
  | Lens_Arr   ArrayLens
  | Lens_This


data RecordLens = AtLabel T.Text Lens


data ArrayLens = AtIndex Int Lens


