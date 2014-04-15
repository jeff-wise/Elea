

module Elea.Lang.Sem.System where



import Elea.Prelude

import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.Value

import Elea.Lang.Sem.Lens
import Elea.Lang.Sem.Types
import qualified Elea.Lang.Sem.ValueIndex as VI
import qualified Elea.Lang.Sem.TypeIndex as TI



import Control.Concurrent.STM
import Control.Monad (guard, forM)

import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.List.Stream as L
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
     (AP _ signals eventClass eventMapVar reactions) = do
  eventId ← getEventId
  eventVar ← getEventVar eventId
  event ← readTVar eventVar
  -- Update Event 
  case updateEvent event of
    -- Complete: delete event variable, return parameters
    Left  paramMap → do 
      modifyTVar eventMapVar $ HMS.delete eventId
      return $ Just $ Effect paramMap reactions
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
    updateEvent eventHM =
      let eventHM' = HMS.insert signal (Just value) eventHM
      in  if L.all isJust $ HMS.elems eventHM'
            then Left $ HMS.map fromJust eventHM'
            else Right eventHM'




-------------------- ADD PARTICLE --------------------------

-- | Add a particle to a system, provided the particle
-- has a unique value
addParticle ∷ TVar ParticleDB → Particle → STM ()
addParticle particleDBVar part@(Part partVal) = do
  particleDB ← readTVar particleDBVar
  case insertParticle particleDB of
    Just particleDB' → writeTVar particleDBVar particleDB'
    Nothing          → throwSTM DuplicateParticle

  where

    -- | Insert a particle into the database.
    -- If particle of exact same value already exists, this
    -- function returns Nothing.
    insertParticle ∷ ParticleDB → Maybe ParticleDB
    insertParticle (ParticleDB valIdx partHM) = do
      guard $ not $ HMS.member partVal partHM
      return $ ParticleDB
        (VI.insert partVal valIdx )
        (HMS.insert partVal part partHM)




---------------------- RESPONSES ------------------------

-- | Responses
-- Given a value, returns the signals which are triggered
-- as a result of that value being created in the same system.
responses ∷ TVar ReceptorDB → Value → STM [Signal]
responses receptorDBVar value = do
  receptorDB ← readTVar receptorDBVar
  return $ responses' receptorDB
      
  where

    responses' ∷ ReceptorDB → [Signal]
    responses' (ReceptorDB tyIdx recpMap) =
      let getSignal ty =  case fromJust $ HMS.lookup ty recpMap of
                            (Recp signal) → signal
      in  L.map getSignal $ HS.toList $ TI.lookup value tyIdx


