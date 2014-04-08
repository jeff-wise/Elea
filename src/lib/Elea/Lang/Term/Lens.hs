

module Elea.Lang.Exp.Lens where


------------------------- LENS -----------------------------


data Lens = 
    Lens_Rec   RecordLens
  | Lens_Arr   ArrayLens
  | Lens_This


data RecordLens = AtLabel Text Lens


data ArrayLens = AtIndex Int Lens


