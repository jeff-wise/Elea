

module Elea.Lang.Sem.Force.Synthesis
  ( -- * Synthesis
    synthesize
    -- ** Transform
  , transform
    -- ** Project
  , project
  ) where




import Elea.Prelude

import Elea.Lang.Sem.Lens
import Elea.Lang.Sem.System
import Elea.Lang.Sem.Types
import Elea.Lang.Sem.Transform.Template

import Elea.Lang.Term.Force
import Elea.Lang.Term.System
import Elea.Lang.Term.Transformer
import Elea.Lang.Term.Value


import Control.Concurrent.STM (STM)
import Control.Monad (forM, mapM_)

import qualified Data.List.Stream as L
import Data.Maybe (fromJust)



-- | Synthesize a new particle
synthesize ∷ Universe → ParamMap → Synthesis → STM ()
synthesize univ paramMap (Syn transformer projs) = do
  -- Run transformer to build new value
  value ← transform univ paramMap transformer
  -- Project each value
  mapM_ (project univ value) projs




-- | Transform
transform ∷ Universe → ParamMap → Transformer → STM Value
transform _ paramMap transformer =
  case transformer of
    Tr_Template temp  → return $ template paramMap temp 
--  Tr_Equation eq    → evalEquation paramMap
--  Tr_Query    query → evalQuery findSystem paramMap




-- | Project
project ∷ Universe → Value → Projection → STM ()
project univ value (Projection lens systemId) = do
  let projValue = fromJust $ get lens value
  let targetSystem = lookupSystem systemId univ
  addParticle targetSystem $ ParticleDef projValue
  signals ← reactions targetSystem projValue
  effects ← L.concat <$> forM signals
                (\s → broadcast targetSystem s projValue)
  mapM_ (queueEffect univ) effects


