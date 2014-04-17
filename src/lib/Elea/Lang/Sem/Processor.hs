

module Elea.Lang.Sem.Processor where



import Elea.Prelude

import Elea.Lang.Sem.Types
import Elea.Lang.Sem.Force.Synthesis

import Elea.Lang.Term.Force


import Control.Concurrent.STM (atomically, readTQueue)
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)




-- | Processor
-- The processor runs in its own thread, waiting on a queue
-- of effects. It evaluates each effect, by applying the
-- force in the assigned context (the parameter map).
processor ∷ Universe → IO ()
processor univ@(Univ _ forceMap effectQueue) = forever process

  where
    
    process ∷ IO ()
    process = do
      (Effect paramMap forceIds) ← atomically $ readTQueue effectQueue
      forM_ forceIds (\forceId → do
        let force = lookupForce forceId forceMap
        case force of 
          F_Syn synth  → forkIO $ atomically $ 
                            synthesize univ paramMap synth
          F_IO  action → forkIO $ evalAction paramMap action
        )


evalAction ∷ ParamMap → IOAction → IO ()
evalAction paramMap (IOPerform action) = action paramMap


