


module Elea.Lang.System where





--type SystemMap = TypeMap (TVar System)


--  , _partMap    ∷  ParticleMap
  --, _program    ∷  Program  
--  , _systemMap  ∷  SystemMap
--  , _parentChan ∷  TChan System


--type ParticleMap = TypeMap Particle

{-
data Program = Program 
  { _onInit   ∷  OnInit 
  , _onRecv   ∷  OnRecv
  , _onCreate ∷  OnCreate
  }
-}




{-
mass = create universe >> return ()

-- Events
type OnInit   = [Action]
type OnRecv   = TypeMap [Action]
type OnCreate = TypeMap [Action]




create ∷ System → IO Val
create system = do
  sysVar ← newTVarIO system
  let initActions = system ^. program.onInit
  mapM_ (forkIO . act sysVar) initActions
  system' ← readTVarIO sysVar
  return $ system' ^. sysVal



-- run an action
act ∷ TVar System → Action → IO ()



data Action = 
    Spawn Particle
  | Send Text Particle
  | Terminate
  | Create System
  | Exec Computation


-- kill running threads ..
-- should terminate most systems
-- by simply not calling more actions
-- use this if have threads waiting or 
-- processing and want to kill early?
terminate ∷ System → IO Val



-- TODO which systems are in scope ??
send ∷ Particle → SystemId → System → IO ()
send particle systemId system =
  mapM_ (recv particle) $
    lookup $ typeOf systemId $ system ^. systemMap



recv ∷ Particle → System → IO ()
recv particle system = do
  let allow = lookup (typeOf $ particle ^. partId) $  -- actual kind match
                system ^. program.onRecv
  in  if allow  then spawn particle system
                else return ()  -- TODO create error / log particle ?


-- Sets
-- Define types of particles
-- Just synonyms for static part of particle IDs 

data Set = Set 
  { _setName    ∷  Text
  , _setType    ∷  Type
  }
 


spawn ∷ Particle → System → System
spawn particle@(Particle particleId _ _) 
         system@(System _ partMap _) = 
  let partMap' = insertTy (typeof particleId) particle partMap
      actions = 
  in  system { _partMap = partMap' }




-- checking recv types
-- create particle in system


-- Top level system
-- Contains built-in systems
data Universe = Universe
  { _defSystems ∷  SystemMap
  , _custSystem ∷  SystemMap
  }



-}
