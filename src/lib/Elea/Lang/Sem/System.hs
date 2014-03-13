

module Elea.Lang.Sem.System where



import Elea.Lang.Term.System


import qualified Data.HashMap.Strict as HMS



-- | Global Variables

-- | Every thread accesses the event map
-- if it needs to run a continuation
eventMapVar ∷ STM (TVar (HMS.HashMap Event Bool))
eventMapVar = newTVar HMS.empty


-- | Every thread accesses the universe
universeVar ∷ (STM (TVar System))
universeVar = error "Universe does not exist!"





type EventMap = HMS.HashMap Event Bool





  
waitForDeps ∷ [Event] → STM ()
waitForDeps depEvents = do
  eventMap ← readTVar eventMapVar
  check $ verifyDeps depEvents eventMap
  where
    verifyDeps ∷ [Event] → EventMap → Bool 
    verifyDeps depEvents eventMap =
      let mEvOccs = mapM (flip HMS.lookup $ eventMap) depEvents
      in  case mEvOccs of
            Just evOccs → and evOccs
            Nothing     → False





effectThread ∷ ActionContext → Effect → IO ()
effectThread ac (Effect deps occs sups force) = do
  atomically waitForDeps
  (effects, ts') ← evalForce ts force
  atomically finishEffect
    



finishEffect ∷ STM ()
finishEffect = do
  let updateOccurrences evMap = L.foldl' $ 
        (\ev evMap → HMS.insert ev True evMap) evMap occs
      updateSuppressions evMap = L.foldl' $ 
        (\ev evMap → HMS.insert ev False evMap) evMap sups
  in  modifyTVar eventMapVar
        (updateOccurrences >>> updateSuppressions)
      
      


evalForce ∷ ThreadState → Force → IO ()
evalForce ts force =
  case force of
    Force_Create loc membCons →
      atomically $ -- eval monad create ts loc membCons




create ∷ Location → Cons_Membrane → EffectThread ()
create loc membCons = do
  let parentSys = find loc
  -- verify parent sys exists
  membrane ← consMembrane membCons
  -- verify constraints of parent sys
  let newSystem = System membrane emptyInterior
  -- add system
  -- calculate interaction effects





consMembrane ∷ Cons_Membrane → Action Membrane
consMembrane (Cons_Membrane valCons mInterCons cnstrsCons) = do
  value ← consValue valCons
  mInteraction ← consInteraction <$> mInterCons
  constraints ← consConstraints cnstrsCons
  return $ Membrane value mInteraction constraints



consValue ∷ Cons_Value → Field Value
consValue (Cons_Value syn) = synthesize syn



consInteraction ∷ Cons_Interaction → Field Interaction
consInteraction (Cons_Interaction causeCons effects) =
  Interaction <$> (consCause causeCons) <*> (return effects)




consConstraints ∷ Cons_Constraints → Constraints
consConstraints (Cons_Constraints → Constraints


consCause ∷ Cons_Cause → EffectThread Cause






-- TODO revise after more work
find ∷ Context → Location → STM (Maybe (TVar System))
find (Context mainSys currSysVar _) location =

  case location of
    -- Search from main system
    (Absolute tyList) → search tyList mainSys
    -- Search from current system
    (Relative tyList) → search tyList currSysVar
    -- Return current parent
    (Parent         ) → view sysParent <$> readTVar currSysVar
    -- Return current system
    (Here           ) → return $ Just currSysVar

  where

    find' ∷ [Type] → TVar System → STM (Maybe (TVar System))
    -- Done searching, returned matched systems
    find' []          sysVar = return $ Just sysVar
    -- Done search, returned matched systems
    find' (ty:remTys) sysVar = do
      system     ← readTVar sysVar
      childIndex ← readTVar (system^.sysChildIndex)
      childMap   ← readTVar (system^.sysChildMap)
      case HS.toList $ VI.lookup ty childIndex of
        []           → return Nothing
        (valOfSys:_) → search remTys $
                          fromJust $ HMS.lookup valOfSys childMap
                            
       




