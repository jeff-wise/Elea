

module Elea.Lang.Exp.FunTree where


import Elea.Prelude
import Elea.Lang.Exp.Types





eval ∷ Env → Fun → Val
eval queryEnv (Fun fun params) = 
  let funEnv = HMS.fromList $ L.map eval' params
  in  evalFun funEnv fun



eval' ∷ Env → FunTree → NamedVal
eval' queryEnv (Fetch valName         ) =
  case HMS.lookup valName queryEnv of
    Just val → NamedVal name val
    Nothing  → NamedVal
                  (Val_Null)
                  (Val_Err $ Err_Fun $ NamedValNotInEnv name)
eval' queryEnv (Fun resName fun params) =
  let funEnv = HMS.fromList $ L.map eval' params
      funVal = evalFun funEnv fun
  in NamedVal resName funVal



evalFun ∷ Env → Function → Val
evalFun env functionTy = 
  case functionTy of
    (ValT template) → evalValTemplate template env
    (Prim funName ) → evalPrim funName env


