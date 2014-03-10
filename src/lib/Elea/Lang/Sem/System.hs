

module Elea.Lang.Sem.System where



import Elea.Lang.Term.System


data Context = Ctx System Val
             -- Log ?
             -- monad instance?

  { _universe   ∷  Universe
  , _currSysVar ∷  TVar System
  , _trigSysVal ∷  Val
  }


-- effects must be executed in IO monad
-- so pass them out of stm monad.


evalEffects ∷ EffectQueue → IO ()  

-- run each effect in order 
-- make sure higher priority ones *complete* first
-- how?



create ∷ Location → Cons_Membrane → STM Context
create loc membCons = do
  let parentSys = find loc -- maybe 
      membrane = consMembrane membCons
      newSystem = System membrane emptyInterior

-- have new system, that's easy
-- make sure parent system exists, throw error
-- make sure parent membrane allows new system
-- given value of new system, calculate effects of parent system

  
  


-- how to do this?
update ∷ Location → Cons_Membrane → STM Context




-- need to know if create/update ?
-- find parent system


data Cons_System =
  Cons_System URITy Cons_Membrane



-- | Membrane Constructor
data Cons_Membrane =
  Cons_Membrane
    Cons_Value
    (Maybe Cons_Interaction)
    Cons_Constraints




consMembrane ∷ Cons_Membrane → Membrane
consMembrane (Cons_Membrane valCons mInterCons cnstrsCons) = 
  let value = consValue valCons
      mInteraction = consInteraction <$> mInterCons
      constraints = consConstraints cnstrsCons
  in  Membrane value mInteraction constraints
      



consValue ∷ Context → Cons_Value → Value
consValue ctx (Cons_Value valSyn) = synthesize ctx valSyn



consInteraction ∷ Cons_Interaction → Interaction


consConstraints ∷ Cons_Constraints → Constraints


consCause ∷ Cons_Cause → Cause



addSystem ∷ 





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
                            
       

