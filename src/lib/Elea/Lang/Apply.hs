

---------------------------------------------------------------------
-- | 
-- Implements all of the built-in functions and a
-- generic apply function.
--
-- The provided functions are mostly wrappers around
-- Haskell functions.
---------------------------------------------------------------------
module Elea.Lang.Apply (apply) where


import Elea.Prelude
import Elea.Lang.Types
import Elea.Lang.Type

import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as Set

import qualified Data.Text as T


-- | Mapping of Elea functions to Haskell functions
--
-- Elea functions are more strongly typed than 
-- Haskell functions, and are used primarily for
-- data transformation than to measure properties or
-- flow control (what types are used for).
funDict ∷ FunDict
funDict = HMS.fromList $ [
    ( "SetSize"
    , FunDef  ([Ty_Set AnySet])
              (Ty_And $ AndTy (Ty_Num Natural) (Ty_Num NonNegative))
              fun_SetSize
    )
  ]



apply ∷ T.Text → [Val] → Val
apply funName paramVals =
  case HMS.lookup funName funDict of
    Just (FunDef paramTys _ fun) →
      let eParams = for (zip paramTys paramVals) (\(ty, val) →
                      if isType ty val
                        then Right val
                        else Left $ UnexpectedParamType ty val 
                    )
      in  case partitionEithers eParams of
            ([], params)  → fun params
            _             → Val_Err $ Err_App $ AppParamError eParams
    Nothing → Val_Err $ Err_App AppFunNotFound



fun_SetSize ∷ [Val] → Val
fun_SetSize [Val_Set (Set set)] = Val_Num $ Z $ Set.size set
fun_SetSize _                   = Val_Err $ Err_App ExecError



