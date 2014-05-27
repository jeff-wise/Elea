

module Elea.Lang.Term.Force where


import Elea.Prelude

import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.Lens
import Elea.Lang.Term.Transformer
import Elea.Lang.Term.Value


import qualified Data.HashMap.Strict as HMS


data Force =
    F_Syn Synthesis
  | F_IO  IOAction


data Synthesis = Syn Transformer [Map]


data Map = Map Lens Projection


data Projection = Projection Location Method


data Location = Location URL SystemId

data URL = URL 

data Method = Genesis | Mutation ValueId Lens


data IOAction = IOPerform ((HMS.HashMap Signal Value) → IO ())



