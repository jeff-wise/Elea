


module Elea.Lang.Exp.Synthesis (
    synthesize                            
  ) where



import Elea.Prelude
import Elea.Lang.Atom.Lens
import Elea.Lang.Atom.Types
import Elea.Lang.Fun.Apply
import Elea.Lang.Exp.Types
import Elea.Lang.Exp.Query


import Control.Error.Util (note)
import Control.Monad (mapM)

import qualified Data.Foldable as F
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import qualified Data.Text as T




-- | Synthesize a system by combining values
-- from many adjacent systems
synthesize ∷ Context → Synthesis → STM Val
synthesize ctx (Syn synId queries apps temp) = do
  -- | Run queries to get data columns
  cols ← Seq.fromList <$> mapM (query ctx) queries
  let env = HMS.fromList $ F.toList $
              Seq.mapWithIndex (runApp synId cols) apps
  return $ eval env temp



-- Apply function over data columns
runApp ∷ T.Text → (Seq.Seq Val) → Int →
          Application → (Val, Val)
runApp synthId cols appIdx (App _ params funName resName) =
  let -- Find parameter values in data sources
      eParams = (getParamVal cols) <$> params
      -- Create an app error value for this synthesis
      mkErrVal = Val_Err . Err_Syn (Text synthId) 
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
getParamVal ∷ (Seq.Seq Val) → Param → Either ParamError Val
getParamVal cols (Param colIndex lens) = do
  col ←  if colIndex >= 0 && colIndex < Seq.length cols
            then  Right $ Seq.index cols colIndex  
            else  Left RefColDoesNotExist
  note ParamLensNotFound $ get lens col



