

module Elea.Lang.Relation where


import Data.HashMap.Lazy as HL
import Data.Set as S

{-

-- Have to first create a relation
newRelation ∷ RelDef → RelIndex → Maybe RelMap
newRelation relDef relMap =
  if HS.member relDef relMap
    then  Nothing
    else  Just $ insert relDef $ HS.empty
          

-- Insert a new tuple into an existing relation
insert ∷ RelDef → RelTuple → RelMap → Maybe RelMap
insert relDef (domElem, codElem) relMap = do
  let insertRel rel = if HS.member domElem rel
                        then adjust (codElem:) domElem rel
                        else insert domElem [codElem] rel
  in  lookup relDef relMap >>= return . insertRel
 

related ∷ RelDef → ParticleKey → RelMap → [ParticleKey]
related relDef dom relMap = lookup relDef relMap >>= lookup dom


relQuery ∷ RelMap → RelDef → Type → [Particle]
relQuery relMap relDef partTy = 

-}

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


