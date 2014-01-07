


module Elea.Lang.Synthesis where


import Elea.Prelude

import Elea.Lang.Types
import Elea.Lang.Apply (apply)
import Elea.Lang.Constructor (construct)
import Elea.Lang.Lens (get)
import Elea.Lang.Val (eval)


import Control.Error.Util (note)

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq



synthesize ∷ System → Synthesis → Particle
synthesize system (Syn synName cons apps
                    (Particle particleId particleVal)) = 
  let -- Get result variables, could be error values
      synEnv = HMS.fromList $ F.toList $
                Seq.mapWithIndex runApp apps
      env    = synEnv `HMS.union` (system^.sysEnv)
      synth  = eval env
  in  Particle (synth particleId) (synth particleVal)
  where
    -- Evaluate data columns
    cols = Seq.fromList $ fmap (construct system) cons
    -- Get variable result of function application
    runApp ∷ Int → Application → (Val, Val)
    runApp appIdx (App _ params funName resName) =
      let -- Find parameter values in data sources
          eParams = getParamVal <$> params
          -- Create an app error value for this synthesis
          mkErrVal = Val_Err . Err_Syn (Text synName) 
                             . SynAppError (Z appIdx)
          -- Calculate the result value if all params found
          -- or return an Application Parameter error
          resVal = case partitionEithers eParams of
            -- No errors, try to apply function
            ([], paramVals) → case apply funName paramVals of
                              Left  appErr → mkErrVal appErr
                              Right val    → val
            -- Some errors, return all parameters
            _            → mkErrVal $ AppParamError eParams  
      in  (resName, resVal)

    -- Find param value in data columns
    getParamVal ∷ Param → Either ParamError Val
    getParamVal (Param colIndex lens) = do
      col ←  if colIndex >= 0 && colIndex < Seq.length cols
                then  Right $ Seq.index cols colIndex  
                else  Left RefColDoesNotExist
      note ParamLensNotFound $ get lens col






