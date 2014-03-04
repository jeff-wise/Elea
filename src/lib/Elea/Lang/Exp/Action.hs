


module Elea.Lang.Exp.Action where


import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Exp.Synthesis
import Elea.Lang.Exp.Types
import Elea.Lang.Exp.Universe
import qualified Elea.Lang.Atom.Index.Val as VI


import Control.Error.Util ((??))
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Monad (mapM_, guard)

import qualified Data.HashMap.Strict as HMS
import qualified Data.List.Stream as L




runAction ∷ Context → Action → IO ()
runAction context action =
  case action of
    (Action_Con constructor) → constructIO context constructor




-- | Create a new system via synthesis
-- and send it to the target system
constructIO ∷ Context → Constructor → IO ()
constructIO context constructor = do
  consResult ← atomically $ runEitherT $
                  construct context constructor
  case consResult of
    Right (ConsEvent initThreads parentThreads) →
          mapM_ forkIO initThreads
       >> mapM_ forkIO parentThreads
    Left  univErr       →
        let errVal = Val_Err $ Err_Univ univErr
        in  atomically $ report errVal




construct ∷ Context → Constructor →
             EitherT UnivError STM ConsEvent
construct ctx@(Ctx univ _ _)
          (Con initActions prog synth parentSysLoc) = do
  -- (1) Locate the target system
  parentSysVar ← (lift $ findSystem ctx parentSysLoc) >>=
                    (?? CannotFindSystemLocation)
  -- (2) Read target system values
  parentSys         ← lift $ readTVar parentSysVar
  parentSysChildMap ← lift $ readTVar (parentSys^.sysChildMap)
  parentSysProg     ← lift $ readTVar (parentSys^.sysProgram)
  -- (3) Create the new system
  newSysVal ← lift $ synthesize ctx synth
  newSysVar ← lift $ newSystemVar prog newSysVal (Just parentSysVar)
  -- (4) Verify new system is unique in parent system
  (guard $ not $ HMS.member newSysVal parentSysChildMap)
    ?? SystemAlreadyExists
  -- (5) Get parent system reactions -- if there are
  --     no reactions, then system is not permitted
  parentActions ← reactions parentSysProg newSysVal
                    ?? SystemNotPermitted
  -- (6) Install new system in the parent system
  lift $ modifyTVar (parentSys^.sysChildIndex) $
          VI.insert newSysVal
  lift $ modifyTVar (parentSys^.sysChildMap) $
          HMS.insert newSysVal newSysVar
  -- (7) Get the events
  let initCtx = Ctx univ newSysVar newSysVal
      parentCtx = Ctx univ parentSysVar newSysVal
  return $ ConsEvent {
      _initNewSysThrds = L.map (runAction initCtx) initActions
    , _parSysThrds     = L.map (runAction parentCtx) parentActions
    }



