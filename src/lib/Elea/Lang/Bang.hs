

module Elea.Lang.Bang where



import Elea.Prelude
import Elea.Lang.Exp.Types
import Elea.Lang.Exp.Action
import Elea.Lang.Exp.Universe


import Control.Monad (mapM_)




bang ∷ System → [Action] → IO Universe
bang mainSys initActions = do
  univ ← atomically $ newUniverse mainSys
  mainSysVal ← atomically $ univ^.mainSys.sysVal
  let initActions' = runState (evalActions initActions) 
                              (univ^.symTable)
      initCtx = Ctx {
                  _universe   = univ
                , _currSysVar = univ^.sysMain
                , _trigSysVal = mainSysVal
                }
  mapM_ (forkIO . runAction initCtx) initActions
  return univ

