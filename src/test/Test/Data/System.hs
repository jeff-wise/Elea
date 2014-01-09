

--------------------------------------------------
-- |
--------------------------------------------------
module Test.Data.System (
  -- * System
    testSystem
  -- * Symbols
  , sym_rpg, sym_color, sym_social
  ------ Colors
  , Sym_Color, blue, red, yellow
  ------ Social
  , Sym_Social, username, friends
  ------ RPG
  , Sym_RPG, entity, attr, domain
  --------- Attributes
  , Sym_Attribute
  , charClass, health, gold, name, books, charDomains
  --------- Entities
  , Sym_Entity
  , dungeon, hero, rogue, warlock, dragon
  --------- Domains
  , Sym_Domain
  , earth, fire, air, death
  ) where
 



import Test.Prelude

import Elea.Lang.Types


import Control.Monad.State.Lazy (runState)

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as HMS



---------------------------------------------------------------------
-- Symbols
---------------------------------------------------------------------

initTestSystem = System {
    _sysVal   = Val_Set $ Set $ Set.empty
  , _sysEnv   = HMS.empty
  , _symTable = newSymbolTable
}


---------------------------------------------------------------------
-- Symbols
---------------------------------------------------------------------


-- | Colors 
--------------------------------------------------
data Sym_Color = Sym_Color
  { _blue   ∷  Symbol
  , _red    ∷  Symbol
  , _yellow ∷  Symbol
  }


-- | Symbols for a Social Application
--------------------------------------------------
data Sym_Social = Sym_Social
  { _username ∷  Symbol
  , _friends  ∷  Symbol
  }


-- | Symbols for an RPG
--------------------------------------------------
data Sym_RPG = Sym_RPG
  { _entity ∷  Sym_Entity
  , _attr   ∷  Sym_Attribute
  , _domain ∷  Sym_Domain
  }

data Sym_Attribute = Sym_Attribute
  { _charClass    ∷  Symbol
  , _health       ∷  Symbol
  , _gold         ∷  Symbol
  , _name         ∷  Symbol
  , _books        ∷  Symbol
  , _charDomains  ∷  Symbol
  }

data Sym_Entity = Sym_Entity
  { _dungeon    ∷  Symbol
  , _hero       ∷  Symbol
  , _rogue      ∷  Symbol
  , _warlock    ∷  Symbol
  , _dragon     ∷  Symbol
  }

data Sym_Domain = Sym_Domain
  { _earth  ∷  Symbol
  , _fire   ∷  Symbol
  , _air    ∷  Symbol
  , _death  ∷  Symbol
  }



-- Create the Color symbols
--------------------------------------------------
mkColorSyms = do 
  blue   ← newSymbol "blue"
  red    ← newSymbol "red"
  yellow ← newSymbol "yellow"
  return $ Sym_Color blue red yellow


-- Create the Social symbols
--------------------------------------------------
mkSocialSyms = do
  username ← newSymbol "username"
  friends  ← newSymbol "friends"
  return $ Sym_Social username friends


-- Create the RPG symbols
--------------------------------------------------
mkAttrSyms = do 
  a ← newSymbol "Character Class"
  b ← newSymbol "Health"
  c ← newSymbol "Gold"
  d ← newSymbol "Name"
  e ← newSymbol "Domain"
  f ← newSymbol "Top Three Favorite Books (Ranked)"
  return $ Sym_Attribute a b c d e f

mkEntitySyms = do 
  a ← newSymbol "Dungeon"
  b ← newSymbol "Hero"
  c ← newSymbol "Rogue"
  d ← newSymbol "Warlock"
  e ← newSymbol "Dragon"
  return $ Sym_Entity a b c d e

mkDomainSyms = do 
  a ← newSymbol "Earth"
  b ← newSymbol "Fire"
  c ← newSymbol "Air"
  d ← newSymbol "Death"
  return $ Sym_Domain a b c d

mkRpgSyms = do 
  x ← mkEntitySyms
  y ← mkAttrSyms
  z ← mkDomainSyms
  return $ Sym_RPG x y z



-- Create all the symbols
-- and return them as top-level values
--------------------------------------------------
mkSymbols = do
  colors ← mkColorSyms
  rpg    ← mkRpgSyms
  social ← mkSocialSyms
  return (colors, rpg, social)


( (sym_color, sym_rpg, sym_social)
  , testSystem ) = runState mkSymbols initTestSystem



-- Create lenses for all the symbol collections
--------------------------------------------------
makeLenses ''Sym_Social
makeLenses ''Sym_Color
makeLenses ''Sym_RPG
makeLenses ''Sym_Entity
makeLenses ''Sym_Attribute
makeLenses ''Sym_Domain





