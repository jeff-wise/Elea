

-- | Universe
-- A Universe contains all the information about an
-- Elea application. It is the Elea equivalent as a 
-- library, is used to represent a standalone Elea
-- application, or a collection of systems, types etc.
-- that can be shared.
module Elea.Lang.Term.Universe where



import qualified Data.HashMap.Strict as HMS



data Universe = Universe
  { _univSystems      ∷  HMS.HashMap URI Cons_System
  , _univConstraints  ∷  HMS.HashMap URI Cons_Constraints
  , _univInteractions ∷  HMS.HashMap URI Cons_Interaction
  , _univValues       ∷  HMS.HashMap URI Synthesis
  , _univTypes        ∷  HMS.HashMap URI Synthesis
  }
