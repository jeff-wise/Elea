

-- | System Terms
--
-- These are the system components that are used
-- to construct a program.
module Elea.Lang.Term.System
  ( -- * Action Potential
    APDefinition (..), EventClass
    -- * Receptor
  , ReceptorDefintion (..)
    -- * Particle
  , ParticleDefinition (..)
  ) where



import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.Lens
import Elea.Lang.Term.Value
import Elea.Lang.Term.Type





data APDefinition = APDef [Signal] EventClass [ForceId]




-- | Event Class
-- The event class is a reference to a part of a value
-- which can be used to distinguish occurrences of some
-- signal, so that multiple events may be processed
-- concurrently.
type EventClass = Lens





data ReceptorDefintion = ReceptorDef ReceptorId Type






newtype ParticleDefinition = ParticleDef Value

