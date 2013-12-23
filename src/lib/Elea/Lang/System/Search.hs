

module Tao.Lang.Statement.Types where


data Constructor = 
    Con_Query Query
  | Con_Generator Generator



data Query = 
  Query
    -- | Match ParticleIDs 
    Matcher
    -- | Filter matched particles by value
    Type
    -- | Extract values from matched particles
    Type



data Matcher = 
    MatchType Type
  | MatchRel Relation


query ∷ Query → Set
query 






---- TODO
--
data Generator = Generator FinType

-- | List comprehension syntax?
-- Can actually be infinite. language is lazy
generate ∷ Generator → Val

