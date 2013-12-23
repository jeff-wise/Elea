


module Elea.Lang.Synthesis where


import Elea.Prelude

import Elea.Lang.Types
import Elea.Lang.Apply (apply)
import Elea.Lang.Constructor (construct)
import Elea.Lang.Lens (get)
import Elea.Lang.Val (eval)


import Control.Error.Util (note)
import Control.Monad (forM)

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq
import qualified Data.Text as T



data Synthesis = Synthesis
  { _synName  ∷  T.Text
  , _synCons  ∷  [Constructor]
  , _synApps  ∷  Seq.Seq Application
  }


data Application = Application
  { _appName    ∷  Maybe Text
  , _appParams  ∷  [Param]
  , _appFunName ∷  Text
  , _resName    ∷  Text
  }      


data Param = Param
  { _colIndex   ∷  Int
  , _paramLens  ∷  Lens
  } 


type Binding = (Text, Val)


synthesize ∷ System → Synthesis → Particle → Particle
synthesize system (Synthesis synName cons apps) particle = 
  let -- Get result variables, could be error values
      synEnv = HMS.fromList $ F.toList $ Seq.mapWithIndex runApp apps
      sysEnv = get sysEnv system
      synth = eval (sysEnv `union` sysEnv)
  in  synth <$> particle
  where
    -- Evaluate data columns
    cols = Seq.fromList $ map (construct system) cons
    -- Get variable result of function application
    runApp ∷ Int → Application → Binding
    runApp appIdx (Application params func resName) =
      let eParams = map getParamVal params
          resVal = case partitionEithers eParams of
            ([], params) → apply func params 
            _            → Val_Err $ AppParamError eParams  
      in  (resName, resVal)

    -- Find param value in data columns
    getParamVal ∷ Param → Either ParamError Val
    getParamVal (Param colIndex lens) = do
      col ←  if colIndex >= 0 && colIndex < length cols
                then  Right $ Seq.index cols colIndex  
                else  Left RefColDoesNotExist
      note ParamLensNotFound $ get lens col






