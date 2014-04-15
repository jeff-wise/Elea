
{-# LANGUAGE DeriveDataTypeable #-}


module Elea.Lang.Sem.Types where



import Elea.Prelude

import Elea.Lang.Term.Force
import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.System
import Elea.Lang.Term.Type
import Elea.Lang.Term.Value

import Elea.Lang.Sem.TypeIndex as TI
import Elea.Lang.Sem.ValueIndex as VI


import Control.Exception
import Control.Concurrent.STM (STM, TVar, TQueue, writeTQueue)

import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust)
import Data.Typeable



------------------------- SYSTEM ---------------------------


data System = Sys
  { sysParticleDB ∷ TVar ParticleDB
  , sysReceptorDB ∷ TVar ReceptorDB
  , sysSigIdxVar  ∷ TVar SignalIndex
  , sysAPMapVar   ∷ TVar ActionPotentialMap
  }



-- | Signal Index
-- Track which signals map to which action potentials, since
-- APs are most often looked up by the signals they contain
type SignalIndex = HMS.HashMap Signal [ActionPotentialId]



-- | Action Potential Map
-- Store Action Potentials by unique identifier. Track their
-- state (active/inactive) and store the send transaction of
-- each AP, since that is the only runtime operation performed
-- on an Action Potential.
type ActionPotentialMap =
    HMS.HashMap ActionPotentialId ActionPotential



-- | An action potential may choose whether to accept signals.
-- An Inhibited AP loses all current events and ignores all
-- incoming signals.
data ActionPotential = AP
  { apActive      ∷  Bool
  , apSignals     ∷  [Signal]
  , apEventClass  ∷  EventClass
  , apEventMapVar ∷  TVar EventMap
  , apReactions   ∷  [ForceId]
  }


-- | Event
-- An event waits on multiple signals concurrently, and stores
-- the values which caused the signals. When all signals are
-- received, then the action potential is fired and its forces
-- are resolved.
type Event = HMS.HashMap Signal (Maybe Value)



-- | Event Map
-- An instance of an event occurs simulataneously to other
-- events for the same Action Potential.
--
--                            * Event1 needs one more signal
--    Action Potential        * Event2 needs two more signals
--    |    |    |    |        * Event3 received no signals, so
--   ev1  ev2  ev3  ev4         it doesn't actually exist
--   x x  x         xxx       * Event4 will fire the action potential  
--                              but not disrupt Event1 or Event2
--                              
--  Suppose the Event Class is: Record @ "username"
--    then we could map Event1 to signals from { "username" : "bob" }
--    and Event2 to signals from {"username" : "Sara"}
--    and so on...
--
type EventMap = HMS.HashMap EventId (TVar Event)



-- | instance of the event class
type EventId = Value




-- | Receptor Database
-- Index each receptor type, since receptors are most often
-- searched on a value to find which types it matches.
data ReceptorDB =
  ReceptorDB
    TypeIndex
    (HMS.HashMap Type Receptor)


newtype Receptor = Recp Signal


-- | Create a new Receptor Database
newReceptorDB ∷ ReceptorDB
newReceptorDB = ReceptorDB TI.newTypeIndex HMS.empty




-- | Particle Database
-- Index each particle value, since particles are most often
-- queried by type (to find matching values).
-- Store particles in map, for now. The map isn't really used
-- at the moment, other than to check for duplicate values.
data ParticleDB =
  ParticleDB
    ValueIndex
    (HMS.HashMap Value Particle)


newtype Particle = Part Value


-- | Create a new Particle Database
newParticleDB ∷ ParticleDB
newParticleDB = ParticleDB VI.newValueIndex HMS.empty





------------------------ UNIVERSE --------------------------

lookupSystem ∷ SystemId → SystemMap → System
lookupSystem systemId = fromJust . HMS.lookup systemId


lookupForce ∷ ForceId → ForceMap → Force
lookupForce forceId = fromJust . HMS.lookup forceId


queueEffect ∷ EffectQueue → Effect → STM ()
queueEffect = writeTQueue 




data Universe = Univ
  { univSysMap       ∷  SystemMap
  , univForceMap     ∷  ForceMap
  , univEffectQueue  ∷  EffectQueue
  }
  


type SystemMap    = HMS.HashMap SystemId System
type ForceMap     = HMS.HashMap ForceId Force
type EffectQueue  = TQueue Effect





-- | Parameters
type ParamMap = HMS.HashMap Signal Value


-- | Effect
data Effect = Effect ParamMap [ForceId]



data SystemException =
    DuplicateParticle
  | ValueNotInstanceOfEvent
  deriving (Show, Typeable)


instance Exception SystemException
