

-- For now this module exists to eliminate circular module dependencies,
-- but will likely grow on its own in the future
module Elea.Lang.Sem.Database where



import Elea.Lang.Sem.TypeIndex as TI
import Elea.Lang.Sem.ParticleIndex as PI
import Elea.Lang.Sem.Types (Particle, Receptor)


import qualified Data.HashMap.Strict as HMS



-- | Receptor Database
-- Index each receptor type, since receptors are most often
-- searched on a value to find which types it matches.
data ReceptorDB =
  ReceptorDB
    TypeIndex
    (HMS.HashMap Type Receptor)


-- | Create a new Receptor Database
newReceptorDB ∷ ReceptorDB
newReceptorDB = ReceptorDB TI.newTypeIndex HMS.empty





-- | Particle Database
-- Index each particle value, since particles are most often
-- queried by type (to find matching values).
-- Store particles in map, for now. The map isn't really used
-- at the moment, other than to check for duplicate values.
data ParticleDB =
  ParticleDB
    ValueIndex
    (HMS.HashMap ParticleId Particle)




-- | Create a new Particle Database
newParticleDB ∷ ParticleDB
newParticleDB = ParticleDB PI.newParticleIndex HMS.empty


