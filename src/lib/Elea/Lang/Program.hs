

module Elea.Lang.Program where



-- | Program
data Program = Program
  { forceDefs   ∷  [ForceDefinition]
  , systemDefs  ∷  [SystemDefinition]
  , apDefs      ∷  [APDefinition]
  }



data ForceDefinition = ForceDefinition
  { forceId ∷  T.Text
  , force   ∷  Force
  }


data APDefinition = APDefinition
  { actnPotlId ∷  T.Text
  , actnPotl   ∷  ActionPotential
  }


data SystemDefinition = SystemDefinition
  { sysId     ∷  T.Text 
  , sysAPs    ∷  [APId]
  , sysRecps  ∷  [Receptor]
  , sysParts  ∷  [Particle]
  }



-- | Install a program.
install ∷ Program → IO Universe
install p = do


-- need to start processor
--
-- universe interace
--
-- for each system:

