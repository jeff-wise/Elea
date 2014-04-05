

module Elea.Lang.Program where



-- | Program
-- A Program consists of force and system defintions.
data Program = Program
  { forceDefs   ∷  [ForceDef]
  , systemDefs  ∷  [SystemDef]
  }



data ForceDef = ForceDef
  { forceId ∷  T.Text
  , force   ∷  Force
  }


data APDef = APDef
  { actnPotlId ∷  T.Text
  , actnPotl   ∷  ActionPotential
  }


data SystemDef = SystemDef
  { sysId     ∷  T.Text 
  , sysAPs    ∷  [APId]
  , sysRecps  ∷  [Receptor]
  , sysParts  ∷  [Particle]
  }



-- | Install a program.
install ∷ Program → IO Universe
install p = do




