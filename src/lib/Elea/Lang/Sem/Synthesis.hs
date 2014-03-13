


module Elea.Lang.Sem.Synthesis where




---------------------------------------------------------------------
-- 1 Synthesize
---------------------------------------------------------------------

-- 1.1 Types 
---------------------------------------------------------------------

type EvalGraph = HMS.HashMap Int (Either Application Value)
type Node = Int


type EvalNode = StateT EvalGraph EffectThread Value



-- 1.2 Synthesize
---------------------------------------------------------------------


synthesize ∷ Synthesis → EffectThread Value
synthesize (Synthesis rootNode graph) =
  let evalGraph = HMS.map Left graph 
      eval rootNode
  

-- 1.3 Evaluate Node Value
---------------------------------------------------------------------

eval ∷ Node → EvalNode
eval node = get 
       >>= (HMS.lookup node >>> (?? NodeDoesNotExist))
       >>= either evalAppNode return
  

evalAppNode ∷ Application → EvalNode
evalAppNode (Application abs params) =
  mapM eval params >>= (lift . apply abs)



-- 1.4 Apply
---------------------------------------------------------------------

apply ∷ Abstraction → [Value] → EffectThread
apply (Abs_Value val) _ = return val




---------------------------------------------------------------------
-- 1 Synthesize
---------------------------------------------------------------------

query ∷ Query → EffectThread
query (Query 









