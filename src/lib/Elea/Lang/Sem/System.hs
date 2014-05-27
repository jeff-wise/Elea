

module Elea.Lang.Sem.System where



import Elea.Prelude

import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.System
import Elea.Lang.Term.Value

import Elea.Lang.Sem.Lens
import Elea.Lang.Sem.Types
import qualified Elea.Lang.Sem.ValueIndex as VI
import qualified Elea.Lang.Sem.TypeIndex as TI



import Control.Concurrent.STM
import Control.Monad (when, guard, forM, forM_)

import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import Data.List.Stream as L (all, (++))
import Data.Maybe (fromJust)




------------------------- BROADCAST ------------------------

-- | Broadcast Transaction
-- Send a signal to all eligible action potentials.
-- Return any reactions that occur as a result.
broadcast ∷ System → Signal → Value → STM [Effect]
broadcast system signal value = do
  sigIdx ← readTVar $ sysSigIdxVar system
  let apIdList = fromJust $ HMS.lookup signal sigIdx 
  apMap ← readTVar $ sysAPMapVar system
  let apList = fmap (fromJust . flip HMS.lookup apMap) apIdList
  mReactions ← forM apList (\ap →
                  case apActive ap of
                    True  → send signal value ap
                    False → return Nothing
                )
  return $ catMaybes mReactions



--------------------------- SEND ---------------------------


-- | Send Transaction
-- Send a signal to a single action potential.
-- Updates a single event with the signal. A new event
-- may be created, and/or an event may be completed,
-- in which case the action potential fires and the
-- transaction returns a 'Reaction'
send ∷ Signal → Value → ActionPotential → STM (Maybe Effect)
send signal value
     (AP _ signals eventClass eventMapVar forces) = do
  eventId ← getEventId
  eventVar ← getEventVar eventId
  event ← readTVar eventVar
  -- Update Event 
  case updateEvent event of
    -- Complete: delete event variable, return parameters
    Left  paramMap → do 
      modifyTVar eventMapVar $ HMS.delete eventId
      return $ Just $ Effect paramMap forces
    -- Incomplete: Write back into variable
    Right event'   → do
      writeTVar eventVar event' 
      return Nothing


  where


    getEventId ∷ STM EventId
    getEventId =  case get eventClass value of
                    Just eventId → return eventId
                    Nothing      → throwSTM ValueNotInstanceOfEvent


    getEventVar ∷ EventId → STM (TVar Event)
    getEventVar eventId = do
      eventMap ← readTVar eventMapVar
      case HMS.lookup eventId eventMap of
        Just eventVar → return eventVar
        Nothing       → do
          eventVar ← newTVar $ HMS.fromList [(s, Nothing) | s ← signals]
          modifyTVar eventMapVar (HMS.insert eventId eventVar)
          return eventVar


    updateEvent ∷ Event → Either ParamMap Event
    updateEvent event =
      let event' = HMS.insert signal (Just value) event
      in  if L.all isJust $ HMS.elems event'
            then Left $ HMS.map fromJust event'
            else Right event'




-------------------- ADD PARTICLE --------------------------

-- | Add a particle to a system, provided the particle
-- has a unique value
addParticle ∷ Universe → System → ParticleDefinition → STM ()
addParticle univ system (ParticleDef partVal) = do
  uuid ← drawUnique univ
  let particleDBVar = sysPartDbVar system
      -- For now assume particle is always new concept, version = 0
      newParticle   = Part uuid 0 partVal
  modifyTVar particleDBVar $ (\(ParticleDB valIdx partHM) →
    ParticleDB
      (VI.insert partVal valIdx)    
      (HMS.insert uuid newParticle partHM)
  )
  particleDB ← readTVar particleDBVar
  case updateParticleDB particleDB of
    Just particleDB' → writeTVar particleDBVar particleDB'
    Nothing          → throwSTM DuplicateParticle

  where

{- TODO Removal Candidate
    -- | Insert a particle into the database.
    -- If particle of exact same value already exists, this
    -- function returns Nothing.
    updateParticleDB ∷ ParticleDB → Maybe ParticleDB
    updateParticleDB (ParticleDB valIdx partHM) = do
      guard $ not $ HMS.member partVal partHM
      return $ ParticleDB
        (VI.insert partVal valIdx )
        (HMS.insert partVal (Part partVal) partHM)

-}


----------------------- ADD RECEPTOR -----------------------

-- This will likely be changed a lot in the next version,
-- so for now it is pretty much the same as add particle

addReceptor ∷ System → ReceptorDefintion → STM ()
addReceptor system (ReceptorDef recpId recpTy) = do
  let receptorDBVar = sysRecpDbVar system
  receptorDB ← readTVar receptorDBVar
  case updateReceptorDB receptorDB of
    Just receptorDB' → writeTVar receptorDBVar receptorDB'
    Nothing          → throwSTM DuplicateParticle

  where

    -- | Insert a particle into the database.
    -- If particle of exact same value already exists, this
    -- function returns Nothing.
    updateReceptorDB ∷ ReceptorDB → Maybe ReceptorDB
    updateReceptorDB (ReceptorDB typIdx recpMap) = do
      guard $ not $ HMS.member recpTy recpMap
      return $ ReceptorDB
        (TI.insert recpTy typIdx)
        (HMS.insert recpTy (Recp recpId) recpMap)




----------------------- ADD RECEPTOR -----------------------


addActionPotential ∷ System → APDefinition → STM ()
addActionPotential system (APDef apId signals eventClass forces) = do
  apMap ← readTVar $ sysAPMapVar system
  -- If an AP with the same ID already exists, abort
  when (HMS.member apId apMap) $
    throwSTM DuplicateActionPotential
  -- Index the new AP by the signals it listens for
  forM_ signals (\signal →
      -- Associate this AP with each signal in the signal index
      modifyTVar (sysSigIdxVar system) $
        HMS.insertWith (++) signal [apId] 
    )
  newEventMapVar ← newTVar HMS.empty
  let newAP = AP { apActive      = True
                 , apSignals     = signals
                 , apEventClass  = eventClass
                 , apEventMapVar = newEventMapVar
                 , apReactions   = forces
                 }
  modifyTVar (sysAPMapVar system) (HMS.insert apId newAP)




------------------------ RESPONSES -------------------------

-- | Responses
-- Given a value, returns the signals which are triggered
-- as a result of that value being created in the same system.
reactions ∷ System → Value → STM [Signal]
reactions system value = do
  let receptorDBVar = sysRecpDbVar system
  receptorDB ← readTVar receptorDBVar
  return $ reactions' receptorDB
      
  where

    reactions' ∷ ReceptorDB → [Signal]
    reactions' (ReceptorDB tyIdx recpMap) =
      let getSignal ty =  case fromJust $ HMS.lookup ty recpMap of
                            (Recp signal) → signal
      in  fmap getSignal $ HS.toList $ TI.lookup value tyIdx


