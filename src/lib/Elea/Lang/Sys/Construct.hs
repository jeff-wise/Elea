

module Elea.Lang.Sys.Construct where



construct ∷ System → Constructor → STM Val
construct _      (Con_Val val               ) = return val
construct system (Con_Sys                   ) = readTVar system^.sysVal
construct _      (Con_Run systemId envConsts) = run systemId envConsts



-- | Run a system and return its value
run ∷ Val → [(Val, Val)] → STM Val
run systemId envConsts = do
  case findSystem systemId library of
    Just system → do
        mapM_ (uncurry $ addConst system) envConsts 
        init system
    Nothing →  return $ Val_Err $ Err_Proc $
                  CannotFindSystem systemId 



query ∷  
