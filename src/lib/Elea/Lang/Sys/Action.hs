


module Elea.Lang.Sys.Action where


import Elea.Prelude
import Elea.Lang.Atom.Lens (get)
import Elea.Lang.Fun.Apply (apply)
import Elea.Lang.Sys.Types
import Elea.Lang.Sys.System (eval)


import Control.Error.Util (note)

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq




-- | Run an action and trigger appropriate events
act ∷ System → Action → IO ()
act system (Spawn particle     ) = do
  atomically $ spawn particle system
  ev ← atomically $ spawnEvent (particle^.partId) system
  runActions ev
act system (Synth synthesis    ) =
  (atomically $ synthesize system synthesis)
    >>=
  (act system . Spawn)
act _ (Proc  systemId envConsts) =
  atomically $ procSys systemId envConsts
act _ (Send  systemId particle)) =
  atomically $ send systemId particle



-- | Run each action in a separate thread
runActions ∷ Event → IO ()
runActions actions = mapM_ (forkIO act) actions




-----------------------------------------------------------
-- Actions
-----------------------------------------------------------

-- | Add a new 'Particle' to the  'System'
-- If a particle with the same ID already exists, it
-- will be created with a new version number or as 
-- per the current versioning policy.
spawn ∷ Particle → System → STM ()
spawn newPart@(Particle newPartId _) system = do
  partMap ← readTVar $ system^.sysPartMap
  -- Add to index if new id type
  if HMS.member newPartId partMap
    then modifyTVar (system^.sysPartIndex) $ VI.insert newPartId
    else return ()
  -- Place new value at beginning of sequence
  modifyTVar (system^.sysPartMap) $
    HMS.insertWith (><) newPartId (Seq.singleton newPart)
  



procSys ∷ Val → [(Val, Val)] → STM Val
procSys systemId envConsts = do
  library ← readTVar $ universe^.library
  procMap ← readTVar $ universe^.univProcMap
  let eSystem = do
        note (ProcessAlreadyExists systemId)
            (guard $ not $ HMS.member systemId procMap)
        note (CannotFindSystem systemId) 
            findSystem systemId library
  case eSystem of
    Right system → do
        mapM_ (uncurry $ addConst system) envConsts 
        modifyTVar (universe^.univProcMap) $
          HMS.insert systemId system 
        init system
    Left procErr → return $ Val_Err $ Err_Proc procErr




synthesize ∷ System → Synthesis → STM Particle
synthesize system (Syn synName cons apps particle) = do
  systemEnv ← readTVar $ system^.sysEnv
  let -- Get results of applications to constructors
      synEnv = HMS.fromList $ F.toList $
                Seq.mapWithIndex runApp apps
      -- Combine system env with synthesis env
      env    = synEnv `HMS.union` systemEnv
  return $ Particle (eval env $ particle^.partId)
                    (eval env $ particle^.partVal)
  where
    -- Evaluate data columns
    cols = Seq.fromList $ fmap (construct system) cons
    -- Get variable result of function application
    runApp ∷ Int → Application → (Val, Val)
    runApp appIdx (App _ params funName resName) =
      let -- Find parameter values in data sources
          eParams = getParamVal <$> params
          -- Create an app error value for this synthesis
          mkErrVal = Val_Err . Err_Syn (Text synName) 
                             . SynAppError (Z appIdx)
          -- Calculate the result value if all params found
          -- or return an Application Parameter error
          resVal = case partitionEithers eParams of
            -- No errors, try to apply function
            ([], paramVals) → case apply funName paramVals of
                              Left  appErr → mkErrVal appErr
                              Right val    → val
            -- Some errors, return all parameters
            _            → mkErrVal $ AppParamError eParams  
      in  (resName, resVal)

    -- Find param value in data columns
    getParamVal ∷ Param → Either ParamError Val
    getParamVal (Param colIndex lens) = do
      col ←  if colIndex >= 0 && colIndex < Seq.length cols
                then  Right $ Seq.index cols colIndex  
                else  Left RefColDoesNotExist
      note ParamLensNotFound $ get lens col





-- universe has index of system ids
-- on system create events
-- reactive framework
--    event map
--    type index
--

send ∷ Type → Particle → STM ()
send systemTy particle =
  mSystem ← findSystem systemId
  case mSystem of
    Just system → 
    Nothing     → return ()


-- TODO can rename systems in library

-- TODO what if send system does not exist
-- react to errors ?
-- eg. exceptions
-- exception types are defined/finite ?



 -- mapM_ (recv particle) $
 --   lookup $ typeOf systemId $ system ^. systemMap

--recvEv val system



-----------------------------------------------------------
-- Events
-----------------------------------------------------------


initEvent ∷ System → STM Event
initEvent =  (view sysProgram >>> readTVar)
      >=> (view onInit >>> readTVar)
      >=> (\(OnInit actions) → return actions)



spawnEvent ∷ Val → System → STM Event
spawnEvent particleId =
      (view sysProgram >>> readTVar)
  >=> (view onSpawn >>> readTVar)
  >=> (\spawnProg →
        let ruleTys = TI.lookup particleId (spawnProg^.spawnIndex)
        in  return $ getEvents (spawnProg^.spawnEvMap) ruleTys
      )



recvEvent ∷ Val → System → STM Event
recvEvent particleId =
      (view sysProgram >>> readTVar)
  >=> (view onRecv >>> readTVar)
  >=> (\recvProg →
        let ruleTys = TI.lookup particleId (recvProg^.recvIndex)
        in  return $ getEvents (recvProg^.recvEvMap) ruleTys
      )



getEvents ∷ EventMap → [Type] → Event
getEvents evMap types = go types
  where
    go []           = []
    go (ty:remTys)  = case HMS.lookup ty evMap of
                        Just event → event ++ go remTys
                        Nothing    → error "Should not occur"


