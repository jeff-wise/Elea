

module Elea.Lang.Relation where


import Elea.Lang.Types


import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HMS



-- relation makes sense
-- define two sets of particles, however
-- map ones from first set to some in second
--
-- relation events
-- define cat as type
-- whenever part of type cat gets created
-- have option to specify relations


insert ∷ RelDef → ParticleId → ParticleIdSet →
          System → System
insert relDef domElem codElems system =
  let updateRelation = insertWith HS.union domElem codElems
      newRelation = HMS.singleton domElem codElems
  in  system & sysRelMap %~
        \currIndex →
          if HMS.member relDef currIndex
            then  HMS.adjust updateRelation relDef currIndex
            else  HMS.insert relDef newRelation currIndex


-- relations based on types?


lookup ∷ System → ParticleIdSet → RelDef → ParticleIdSet
lookup system idSet relDef =
  case HMS.lookup relDef (system^.sysRelMap) of
    Just relation →
      let addRelated acc nextId = maybe Set.empty id $ 
                                    lookup nextId relation
      in  Set.foldl' addRelated Set.empty idSet
    Nothing       → Set.empty


-- | Functions over the set of particles
-- TODO theory? posets? or Category theory?
data RelQuery = RelQuery ParticleIdSet [RelDef]



-- related system {bob} (RelDef writtenby post user)

-- how does this work
-- specify type for both or one or the other
-- get posts written by *bob*
-- 
--     post   <----   user
-- GET POST WRITTEN BY USER bob
--
-- GET POST WRITTEN BY USER any
--
-- GET USER any THAT WROTE POST today
-- 


