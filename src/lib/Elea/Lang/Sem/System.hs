

module Elea.Lang.Sem.System where



import Elea.Lang.Term.System





----------------------- BROADCAST --------------------------

newBroadcastTrans ∷ BroadcastTrans
newBroadcastTrans = broadcast (newTVar [])


broadcast ∷ TVar APTransMap → Signal → Value → STM [Reaction]
broadcast apListVar signal value = do
  apTransMap ← readTVar apListVar
  mReactions ← forM (elems apTransMap) (\(st, send) →
                  case st of
                    Active    → send signal value
                    Inhibited → return Nothing
                )
  return $ catMaybes mReactions




-------------------------- SEND ---------------------------

newSendTrans ∷ ActionPotential → SendTrans
newSendTrans ap = sendTrans ap (HMS.empty)


sendTrans ∷ ActionPotential → TVar EventMap → 
             Signal → Value → STM (Maybe Reaction)
sendTrans (AP evClass signals actions) eventMapVar signal value = do
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

  where

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

newAddParticleTrans ∷ AddParticleTrans
newAddParticleTrans = 
  let particleDBVar = newTVar $ ParticleDB newValueIndex HMS.empty
  in  addParticleTrans particleDBVar


addParticleTrans ∷ TVar ParticleDB → Particle → STM ()
addParticleTrans particleDBVar (Part partVal) = do
  particleDB ← readTVar particleDBVar
  case insertParticle newParticle particleDB of
    Just particleDB' → writeTVar particleDBVar particleDB'
    Nothing          → throwSTM DuplicateParticle

  where

    -- Should return Nothing on dup
    insertParticle ∷ ParticleDB → Maybe ParticleDB
    insertParticle (PartDB valIdx partHM) = do
      guard $ not $ HMS.member partVal partHM
      return $ ParticleDB
        (VI.insert partVal valIdx )
        (HMS.insert partVal part partHM)
 



------------------------ PROJECT ---------------------------


newProjectTrans ∷ RuntimeI → ProjectTrans
newProjectTrans (RunI appendAction findSystem) = projectTrans

  where

    projectTrans ∷ Projection → Value → STM ()
    projectTrans (Projection lens sysId) value = do
      let valFragment = get lens value
      targetSys ← findSystem univVar sysId
      let (Sys addParticle triggered broadcast) = targetSys
      addParticle $ Part valFragment
      signals ← triggered valFragment
      reactions ← concat <$> forM signals (\signal →
                     broadcast signal valFragment
                   )
      forM reactions $ appendAction . Action targetSys




----------------------- TRIGGERED --------------------------

newTriggeredTrans ∷ TriggeredTrans
newTriggeredTrans = triggeredTrans (newTVar newReceptorDB)


triggeredTrans ∷ TVar ReceptorDB → Value → STM [Signal]
triggeredTrans receptorDBVar value = do
  receptorDB ← readTVar receptorDBVar
  return $ triggered receptorDB
  
  where

    triggered ∷ ReceptorDB → [Signal]
    triggered (RecpDB tyIdx recpMap) =
      let getSignal ty =  case fromJust $ HMS.lookup ty recpMap of
                            (Receptor recpId _) → recpid
      in  L.map getSignal $ TI.lookup value tyIdx



-- Global functions


findSystem ∷ TVar Universe → SystemId → STM System
findSystem univVar sysId = do
  univ ← readTVar univVar
  case HMS.lookup sysId univ of
    Just sys → return sys
    Nothing  → throwSTM SystemNotFound


newTransformTrans ∷ Context → RuntimeI → TransformTrans
newTransformTrans


transformTrans ∷ Context → RuntimeI → TransformTrans

  where
    transform ∷ Context → Transformer → STM Value
    transform ctx transformer =
      case transformer of







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





----------------------- PROCESSOR --------------------------

processor ∷ Program → ForceI → Action → IO ()
processor (ProgramI getForce)
             (ForceI synthesize encode)
             (Action sys (Reaction paramMap forceIds)) = do
  forM forceIds (\forceId → do
    let force = map getForce forceId
        ctx   = Ctx sys paramMap 
    case force of 
      (F_Syn syn) → forkIO $ atomically $ synthesize ctx syn
      (F_Enc enc) → forkIO $ atomically $ encode ctx enc
  )



