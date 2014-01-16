

module Elea.Lang.Sys.System where


import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Sys.Types
import qualified Elea.Lang.Index.Val as VI
import qualified Elea.Lang.Index.Type as TI


import qualified Data.HashMap.Strict as HMS
import qualified Data.List.Stream as L
import qualified Data.Sequence as Seq
import qualified Data.Text as T




defineSymbol ∷ T.Text → TVar System → STM Int
defineSymbol symName systemVar = do
  system   ← readTVar systemVar
  symTable ← readTVar $ system^.sysSymTable
  let (symId, symTable') = updateSymTable symTable
  writeTVar (system^.sysSymTable) symTable' 
  return symId
  where
    updateSymTable symTable@(SymTable count symMap) =
      case L.find ((symName==) . snd) $ HMS.toList symMap of
        -- Symbol already defined, return it
        Just (symId, _) → (symId, symTable)
        -- Create a new int mapping for text symbol
        Nothing         →
            (     count
            ,     symTable
              >$> over tblCount (+1)
              >>> over tblMap (HMS.insert count symName)
            )



addSpawnEvent ∷ Type → [Action] → TVar System → STM ()
addSpawnEvent partTy actions =
  let updateOnSpawn =
          judge (view spawnIndex >>> HMS.member partTy)
            (id)
            (over spawnIndex $ TI.insert partTy)
        >>> 
          over spawnEventMap
            (HMS.insertWith (++) partTy actions)
  in    readTVar
    >=> (view sysProgram >>> readTVar)
    >=> (view onSpawn >>> (flip modifyTVar) updateOnSpawn)



addRecvEvent ∷ Type → [Action] → TVar System → STM ()
addRecvEvent partTy actions =
  let updateOnRecv =
          judge (view recvIndex >>> HMS.member partTy)
            (id)
            (over recvIndex $ TI.insert partTy)
        >>> 
          over recvEventMap
            (HMS.insertWith (++) partTy actions)
  in    readTVar
    >=> (view sysProgram >>> readTVar)
    >=> (view onRecv >>> (flip modifyTVar) updateOnRecv)




addConst ∷ Val → Val → System → STM ()
addConst constId constVal system =
  modifyTVar (system^.sysEnv) $ HMS.insert constId constVal


remConst ∷ Val → System → STM ()
remConst constId system =
  modifyTVar (system^.sysEnv) $ HMS.delete constId





findSystem ∷ SystemId → STM (Maybe System)
findSystem systemId = do
  library ← readTVar $ universe^.library
  return $ HMS.lookup systemId (library^.libSysMap)







addSystemDef ∷ System → Universe → STM ()
addSystemDef system universe = 
  modifyTVar (universe^.library) $
    over libSysMap $
      HMS.insert (system^.sysId) system


addParticleDef ∷ Particle → Universe → STM ()
addParticleDef particle universe =
  modifyTVar (universe^.library) $
    over libPartMap $
      HMS.insert (particle^.partId) particle




eval ∷ Env → Val → Val
eval env val = eval' val
  where
    eval' ∷ Val → Val
    eval' (Val_Set      (Set set )) = Val_Set $ Set $ HS.map eval' set
    eval' (Val_Arr      (Arr arr )) = Val_Arr $ Arr $ fmap eval' arr
    eval' (Val_Var  var@(Var name)) = case HMS.lookup name env of
                                        Just x  → x
                                        Nothing → Val_Var $ var
    eval' leafVal                   = leafVal




{-
recv ∷ Particle → System → IO ()
recv particle system = do
  let allow = lookup (typeOf $ particle ^. partId) $  -- actual kind match
                system ^. program.onRecv
  in  if allow  then spawn particle system
                else return ()  -- TODO create error / log particle ?
-}


