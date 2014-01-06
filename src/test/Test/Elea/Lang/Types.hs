

--------------------------------------------------
-- |
--------------------------------------------------
module Test.Lang.Types (
    primeSet
  , complexSet
  , simplePair
  , complexPair
  , simpleArray
  , testSystem
  ) where


import Test.Prelude

import Elea.Lang.Types

import Control.Monad.State.Lazy (runState)

import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as HMS



initTestSystem = System {
    _sysVal   = Val_Set $ Set $ Set.empty
  , _sysEnv   = HMS.empty
  , _symTable = newSymbolTable
}


---------------------------------------------------------------------
-- Symbols
---------------------------------------------------------------------


-- | Some collections of newSymbols which will be used
-- in other values
data Sym_Colors = Sym_Colors
  { _blue   ∷  Symbol
  , _red    ∷  Symbol
  , _yellow ∷  Symbol
  }



data Sym_Social = Sym_Social
  { _username ∷  Symbol
  , _friends  ∷  Symbol
  }



data Sym_RPG = Sym_RPG
  { _gold   ∷  Symbol
  , _health ∷  Symbol
  , _arcana ∷  Symbol
  , _level  ∷  Symbol
  }


-- | Create the systems in the test system
mkColorSyms = do 
  blue   ← newSymbol "blue"
  red    ← newSymbol "red"
  yellow ← newSymbol "yellow"
  return $ Sym_Colors blue red yellow


mkRpgSyms = do
  gold   ← newSymbol "gold"
  health ← newSymbol "health"
  arcana ← newSymbol "arcana"
  level  ← newSymbol "arcana"
  return $ Sym_RPG gold health arcana level

mkSocialSyms = do
  username ← newSymbol "username"
  friends  ← newSymbol "friends"
  return $ Sym_Social username friends


mkSymbols = do
  colors ← mkColorSyms
  rpg    ← mkRpgSyms
  social ← mkSocialSyms
  return (colors, rpg, social)

(( sym_colors, sym_rpg, sym_social), testSystem) = 
  runState mkSymbols initTestSystem



---------------------------------------------------------------------
-- General Vals
---------------------------------------------------------------------

primeSet ∷ Val
primeSet = Val_Set $ Set $ Set.fromList [
    Val_Num $ Z 2
  , Val_Num $ Z 3
  , Val_Num $ Z 5
  , Val_Num $ Z 7
  , Val_Num $ Z 11
  , Val_Num $ Z 13
  , Val_Num $ Z 17
  ]


complexSet ∷ Val
complexSet = Val_Set $ Set $ Set.fromList [
    Val_Num $ Z 1
  , Val_Text $ Text "A Set Element"
  , Val_Pair $ Pair (Val_Sym $ _gold sym_rpg) (Val_Num $ Z 500)
  , Val_Set $ Set $ Set.fromList [
        Val_Num $ Z 100
      , Val_Text $ Text "One-hundred"
      , Val_Arr $ Arr $ Seq.fromList 
          [Val_Num $ R 1.414, Val_Num $ R 3.14]
      ]
  , simpleArray
  ]



simplePair ∷ Val
simplePair = Val_Pair $ Pair (Val_Sym $ _blue sym_colors)
                             (Val_Num $ Z 100)


complexPair ∷ Val
complexPair = Val_Pair $ Pair
    (Val_Set $ Set $ Set.fromList [ Val_Num $ Z 3
                                  , Val_Num $ R 1.11
                                  , Val_Text $ Text "Bird"
                                  ]
    ) 
    (Val_Pair $ Pair (Val_Sym $ _blue sym_colors) simpleArray) 


simpleArray ∷ Val
simpleArray = Val_Arr $ Arr $ Seq.fromList [
    Val_Text $ Text "Joe"
  , Val_Text $ Text "Bob" 
  , Val_Text $ Text "Lisa"
  , Val_Text $ Text "Mike"
  ]




---------------------------------------------------------------------
-- RPG
---------------------------------------------------------------------

-- rpg theme for most complex tests?

mkCharSyms = do 
  class'  ← newSymbol "Class"
  level   ← newSymbol "Level"
  name ← newSymbol "Name"
  return $ Sym_Colors blue red yellow


mkRpgSyms = do 
  sonra   ← newSymbol "Sonra"
  okran   ← newSymbol "Okran"
  dungeon ← newSymbol "Dungeon"
  favTaverns ← newSymbol "Top Three Favorite Taverns"
  return $ Sym_Colors blue red yellow


mkRpgSyms = do 
  earth ← newSymbol "Earth"
  fire  ← newSymbol "Fire"
  air   ← newSymbol "Air"
  death ← newSymbol "Death"


mkSkillSyms = do 
  tallDwarf ← newSymbol "Teleport"
  lionsFang ← newSymbol "Eviscerate"
  elixir    ← newSymbol "Fireball"
  illusion  ← newSymbol "Summon"
  spell     ← newSymbol ""




-- what do I need to test with val index
-- same values, try different types, make sure works same
--
-- complex values, try different types, ensure diff results


-- | Minimal particle defintion
dungeon = Particle
  { _partId   = Val_Sym $ _drokahn sym_dungeon
  , _partVal  = Val_Sym $ _dungeon sym_rpg
  }


-- | The hero
hero = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
                    Val_Pair $ Pair (Val_Sym $ _name sym_char)
                                    (Val_Text $ Text "Sonra")
                    Val_Pair $ Pair (Val_Sym $ _level sym_char)
                                    (Val_Num $ Z 12)
                  , Val_Pair $ Pair (Val_Sym $ _class sym_char)
                                    (Val_Sym $ _warrior sym_class)
                  , Val_Pair $ Pair (Val_Sym $ _domain sym_char)
                                    (Val_Set $ Set $ Set.fromList [
                                        Val_Sym $ _fire sym_domain
                                      , Val_Sym $ _air sym_domain
                                      ]
                  ] 
  , _partVal  = Val_Sym $ _hero sym_rpg
  }


warlock = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
                    Val_Pair $ Pair (Val_Sym $ _name sym_char)
                                    (Val_Text $ Text "Okran")
                    Val_Pair $ Pair (Val_Sym $ _level sym_char)
                                    (Val_Num $ Z 15)
                  , Val_Pair $ Pair (Val_Sym $ _class sym_char)
                                    (Val_Sym $ _warlock sym_class)
                  , Val_Pair $ Pair (Val_Sym $ _domain sym_char)
                                    (Val_Set $ Set $ Set.fromList [
                                        Val_Sym $ _earth sym_domain
                                      , Val_Sym $ _fire sym_domain
                                      , Val_Sym $ _death sym_domain
                                      ]
  , _partVal  = Val_Sym $ _okran sym_rpg
  }



dragon = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
                    Val_Pair $ Pair (Val_Sym $ _name sym_char)
                                    (Val_Text $ Text "Ricardo")
                    Val_Pair $ Pair (Val_Sym $ _level sym_char)
                                    (Val_Num $ Z 27)
                  , Val_Pair $ Pair (Val_Sym $ _domain sym_char)
                                    (Val_Set $ Set $ Set.fromList [
                                        Val_Sym $ _fire sym_domain
                                      ]
                                    )





-- how to test
-- generate values of certain types
-- and keep track of values for type
--
--
-- how to ensure good coverage?
--
-- if could generate value, and all corresponding types
-- track # of types, 

-- generate value and corresponding types
-- insert all values
--
-- lookup types, should see value ids 
