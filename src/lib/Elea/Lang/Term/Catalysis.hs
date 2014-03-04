

module Elea.Lang.Term.Catalysis where



data Catalyst = Catalyst
  { _catType  ∷  Type
  , _catReact ∷  Reaction
  }


type Reaction = [Action]

