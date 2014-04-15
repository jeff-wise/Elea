

module Elea.Lang.Term.Force where



import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.Lens
import Elea.Lang.Term.Transformer




data Force = F_Syn Synthesis


data Synthesis = Syn Transformer [Projection]

data Projection = Projection Lens SystemId



