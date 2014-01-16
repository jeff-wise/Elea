


module Elea.Lang.Sys.Action where


import Elea.Prelude
import Elea.Lang.Sys.Types



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




send ∷ SystemId → Particle → STM ()
send systemId particle =
  mSystem ← findSystem systemId
  case mSystem of
    Just system →    
    Nothing     → return ()


-- TODO can rename systems in library

-- TODO what if send system does not exist
-- react to errors ?
-- eg. exceptions
-- exception types are defined/finite ?



  mapM_ (recv particle) $
    lookup $ typeOf systemId $ system ^. systemMap

recvEv val system


