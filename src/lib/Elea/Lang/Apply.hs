

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
import qualified Data.List.Stream as L
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
              (Ty_And $ AndTy (Ty_Num Integer) (Ty_Num NonNegative))
              fun_SetSize
    )
  , ( "Add"
    , FunDef ([Ty_Num $ AnyNumber, Ty_Num $ AnyNumber])
             (Ty_Num $ AnyNumber)
             fun_Add
    )
  ]



apply ∷ T.Text → [Val] → Either AppError Val
apply funName paramVals =
  case HMS.lookup funName funDict of
    Just (FunDef paramTys _ fun) →
      let eParams = (flip fmap) (L.zip paramTys paramVals) (\(ty, val) →
                      if isType ty val
                        then Right val
                        else Left $ IncorrectParamType ty val 
                    )
      in  case partitionEithers eParams of
            ([], params)  → Right $ fun params
            _             → Left $ AppParamError eParams
    Nothing → Left AppFunNotFound



fun_SetSize ∷ [Val] → Val
fun_SetSize [Val_Set (Set set)] = Val_Num $ Z $ Set.size set
-- TODO use custom exception? throw to handling thread?


fun_Add ∷ [Val] → Val
fun_Add [Val_Num x, Val_Num y] = Val_Num $ x + y



