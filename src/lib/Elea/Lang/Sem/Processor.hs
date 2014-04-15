

module Elea.Lang.Sem.Processor where


import Elea.Lang.Sem.Universe
import Elea.Lang.Sem.Force.Synthesis



----------------------- PROCESSOR --------------------------

-- | Processor
-- The processor runs in its own thread, waiting on a queue
-- of effects. It evaluates each effect, by applying the
-- force in the assigned context (the parameter map).
processor ∷ Universe → IO ()
processor univ = forever process

  where
    
    process ∷ IO ()
    process = do
      let getNextEffect = readTQueue $ univEffectQueue univ
      (Effect paramMap forceIds) ← atomically getNextEffect
      forM forceIds (\forceId → do
        let force = map getForce forceId
        case force of 
          (F_Syn syn) → forkIO $ atomically $ synthesize univ paramMap syn
      )


