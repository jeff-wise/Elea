

module Elea.Lang.Sem.System where



import Elea.Lang.Term.System



--- (1) SYSTEM
------------------------------------------------------------
------------------------------------------------------------


----------------------- BROADCAST --------------------------

-- | Create a new Broadcast transaction
newBroadcastTrans ∷ TVar SignalIndex → TVar APMap → BroadcastTrans
newBroadcastTrans sigIdxVar apMapVar = broadcast

  where

    -- | Broadcast Transaction
    -- Send a signal to all eligible action potentials.
    -- Return any reactions that occur as a result.
    broadcast ∷ Signal → Value → STM [Reaction]
    broadcast signal value = do
      sigIdx ← readTVar sigIdxVar
      let apIds = fromJust $ HMS.lookup signal sigIdx 
      apMap ← readTVar apMapVar
      let sendTransList = catMaybes $
                      mapM (flip $ HMS.lookup apMap) apIds
      mReactions ← forM sendTransList (\(apState, sendTrans) →
                      case apState of
                        Active    → sendTrans signal value
                        Inhibited → return Nothing
                    )
      return $ catMaybes mReactions




-------------------------- SEND ---------------------------

-- | Create a new Send transaction
newSendTrans ∷ ActionPotential → TVar EventMap → SendTrans
newSendTrans (AP evClass signals actions) eventMapVar = sendTrans

  where

    -- | Send Transaction
    -- Send a signal to a single action potential.
    -- Updates a single event with the signal. A new event
    -- may be created, and/or an event may be completed,
    -- in which case the action potential fires and the
    -- transaction returns a 'Reaction'
    sendTrans ∷ Signal → Value → STM (Maybe Reaction)
    sendTrans signal value = do
      let evInstance = get evClass value
      eventVar ← getEventVar evInstance eventMap
      event ← readTVar eventVar
      -- Update Event 
      case eventUpdate signal value event of
        -- Complete: delete event variable, return parameters
        Left  paramMap → do 
          modifyTVar eventMapVar (HMS.delete evInstance eventMap)
          return $ Just $ Reaction paramMap actions
        -- Incomplete: Write back into variable
        Right event'   → do
          modifyTVar eventVar event' 
          return Nothing


getEventVar ∷ EventInstance → TVar EventMap → STM (TVar Event)
getEventVar evInstance eventMapVar = do
  eventMap ← readTVar eventMapVar
  case HMS.lookup evInstance eventMap of
    Just eventVar → return eventVar
    Nothing       → do
      let eventVar = newTVar $ HMS.fromList
                      [(signal, Nothing) | signal ← signals]
      modifyTVar eventMapVar (HMS.insert evInstance eventVar)
      return eventVar


eventUpdate ∷ Event → Either ParamMap Event
eventUpdate eventHM =
  let eventHM' = HMS.insert signal (Just value) eventHM
  if all isJust $ elems eventHM'
    then Left HMS.map eventHM' fromJust
    else Right eventHM'




---------------------- ADD PARTICLE ------------------------

newAddParticleTrans ∷ TVar ParticleDB → AddParticleTrans
newAddParticleTrans particleDBVar = addParticleTrans

  where

    addParticleTrans ∷ Particle → STM ()
    addParticleTrans (Part partVal) = do
      particleDB ← readTVar particleDBVar
      case insertParticle newParticle particleDB of
        Just particleDB' → writeTVar particleDBVar particleDB'
        Nothing          → throwSTM DuplicateParticle



-- | Insert a particle into the database.
-- If particle of exact same value already exists, this
-- function returns Nothing.
insertParticle ∷ ParticleDB → Maybe ParticleDB
insertParticle (PartDB valIdx partHM) = do
  guard $ not $ HMS.member partVal partHM
  return $ ParticleDB
    (VI.insert partVal valIdx )
    (HMS.insert partVal part partHM)





----------------------- TRIGGERED --------------------------

-- | Create a triggered transaction
newTriggeredTrans ∷ TVar ReceptorDB → TriggeredTrans
newTriggeredTrans receptorDBVar = triggeredTrans

  where

    -- | Triggered Transaction
    -- Given a value, returns the signals which are triggered
    -- as a result of that value being created in the same system.
    triggeredTrans ∷ Value → STM [Signal]
    triggeredTrans value = do
      receptorDB ← readTVar receptorDBVar
      return $ triggered receptorDB
      

triggered ∷ ReceptorDB → [Signal]
triggered (RecpDB tyIdx recpMap) =
  let getSignal ty =  case fromJust $ HMS.lookup ty recpMap of
                        (Receptor recpId _) → recpId
  in  L.map getSignal $ TI.lookup value tyIdx




----------------------- TRIGGERED --------------------------

newQueryTrans ∷ TVar ParticleDB → QueryTrans
newQueryTrans particleDBVar = queryTrans


queryTrans ∷ Type → STM [Particle]
queryTrans ty = do 
  (PartDB valIdx partMap) ← readTVar particleDBVar
  values ← VI.lookup ty valIdx
  return $ catMaybes $
    mapM (flip $ HMS.lookup partMap) values





--- (2) RUNTIME
------------------------------------------------------------
------------------------------------------------------------


----------------------- FIND SYSTEM ------------------------

findSystem ∷ TVar Universe → SystemId → STM System
findSystem univVar sysId = do
  univ ← readTVar univVar
  case HMS.lookup sysId univ of
    Just sys → return sys
    Nothing  → throwSTM SystemNotFound





------------------------ SYNTHESIS -------------------------

newSynthesizeTrans ∷ RuntimeI → SynthesizeTrans
newSynthesizeTrans runtimeI = synthesizeTrans runtimeI

  where
    
    -- Create transform transaction
    transform = newTransformTrans runtimeI

    -- Create projection transaction 
    project   = newProjectTrans runtimeI
    
    -- Synthesize a new particle
    synthesizeTrans ∷ Context → Synthesis → STM ()
    synthesizeTrans ctx (Synth transformer projs) =  
      -- Run transformer to build new value
      value ← transform ctx transformer
      -- Project each value
      mapM_ (project value) projs



------------------------ TRANSFORM -------------------------

newTransformTrans ∷ RuntimeI → Context → TransformTrans
newTransformTrans (RunI _ findSystem) = transformTrans

  where

    transformTrans ∷ Context → TransformTrans
    transformTrans (Ctx paramMap) transformer =
      case transformer of
        Tr_Template temp   → evalTemplate paramMap temp 
        Tr_Equation eq     → evalEquation paramMap
        Tr_Query    query  → evalQuery findSystem paramMap




------------------------ PROJECT ---------------------------

newProjectTrans ∷ RuntimeI → ProjectTrans
newProjectTrans (RunI appendAction findSystem) = projectTrans

  where

    projectTrans ∷ Projection → Value → STM ()
    projectTrans (Projection lens sysId) value = do
      let valFragment = get lens value
      targetSysI ← findSystem univVar sysId
      (addParticle targetSysI) $ Part valFragment
      signals ← (triggered targetSysI) valFragment
      reactions ← concat <$> forM signals (\signal →
                     (broadcast targetSysI) signal valFragment
                   )
      forM reactions $ appendAction . Action targetSys





--- (3) PROCESSOR
------------------------------------------------------------
------------------------------------------------------------

----------------------- PROCESSOR --------------------------

processor ∷ Program → ForceI → Action → IO ()
processor (ProgramI getForce)
             (ForceI synthesize encode)
             (Action (Reaction paramMap forceIds)) = do
  forM forceIds (\forceId → do
    let force = map getForce forceId
        ctx   = Ctx paramMap 
    case force of 
      (F_Syn syn) → forkIO $ atomically $ synthesize ctx syn
      (F_Enc enc) → forkIO $ atomically $ encode ctx enc
  )


