

module Test.Data.RPG (
    dungeonVal
  , cityVal, castleVal, mineVal
  , bartenderVal, priestVal, beggarVal
  , partySetVal, partyArrVal
  , heroVal, warlockVal, dragonVal
  ) where



import Test.Prelude
import Test.Data.System

import Elea.Lang.Atom.Types


import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set




---------------------------------------------------------------------
-- RPG-Themed Particles
---------------------------------------------------------------------


---------------------------------------------------------------------
-- RPG-Themed Particles
---------------------------------------------------------------------


-- Particle with simple Symbol ID
dungeonVal = Val_Sym $ sym_rpg^.entity.dungeon


-- Particles with simple Text IDs
bartenderVal = Val_Text $ Text "Bartender"


priestVal = Val_Text $ Text "Priest"


beggarVal = Val_Text $ Text "Beggar"



-- Particles with simple Numeric IDs
cityVal = Val_Num $ Z 4


castleVal = Val_Num $ Z (-15)


mineVal = Val_Num $ R 30.3


-- Particle with Array ID
partyArrVal = Val_Arr $ Arr $ Seq.fromList [
    Val_Sym $ sym_rpg^.entity.hero
  , Val_Sym $ sym_rpg^.entity.warlock
  , Val_Sym $ sym_rpg^.entity.rogue
  ]



-- Particle with Set ID
partySetVal = Val_Set $ Set $ Set.fromList [
    Val_Sym $ sym_rpg^.entity.hero
  , Val_Sym $ sym_rpg^.entity.warlock
  , Val_Sym $ sym_rpg^.entity.rogue
  ]



heroVal = Val_Set $ Set $ Set.fromList [
            pair  (Val_Sym $ sym_rpg^.attr.name)
                  (Val_Text $ Text "Hero")
          , pair  (Val_Sym $ sym_rpg^.attr.health)
                  (Val_Num $ Z 100)
          , pair  (Val_Sym $ sym_rpg^.attr.charClass)
                  (Val_Text $ Text "Warrior" )
          , pair  (Val_Sym $ sym_rpg^.attr.charDomains)
                  (Val_Set $ Set $ Set.fromList [
                      Val_Sym $ sym_rpg^.domain.fire
                    , Val_Sym $ sym_rpg^.domain.air
                    ]
                  )
          , pair  (Val_Sym $ sym_rpg^.attr.books)
                  (Val_Arr $ Arr $ Seq.fromList [
                      Val_Text $ Text "Goblins and Ghouls"
                    , Val_Text $ Text "Legends and Lore"
                    , Val_Text $ Text "Swordsmanship 101"
                    ]
                  )
          ]


warlockVal = Val_Set $ Set $ Set.fromList [
              pair  (Val_Sym $ sym_rpg^.attr.name)
                    (Val_Text $ Text "Warlock")
            , pair  (Val_Sym $ sym_rpg^.attr.health)
                    (Val_Num $ Z 251)
            , pair  (Val_Sym $ sym_rpg^.attr.charClass)
                    (Val_Text $ Text "Warlock")
            , pair  (Val_Sym $ sym_rpg^.attr.charDomains)
                    (Val_Set $ Set $ Set.fromList [
                        Val_Sym $ sym_rpg^.domain.earth
                      , Val_Sym $ sym_rpg^.domain.air
                      , Val_Sym $ sym_rpg^.domain.death
                      ]
                    )
            , pair  (Val_Sym $ sym_rpg^.attr.books)
                    (Val_Arr $ Arr $ Seq.fromList [
                        Val_Text $ Text "Navigating Necromancy"
                      , Val_Text $ Text "Legends and Lore"
                      , Val_Text $ Text "Guide to the Underworld"
                      ]
                    )
            ]


dragonVal = Val_Set $ Set $ Set.fromList [
              pair  (Val_Sym $ sym_rpg^.attr.name)
                    (Val_Text $ Text "Dragon")
            , pair  (Val_Sym $ sym_rpg^.attr.health)
                    (Val_Num $ Z 870)
            , pair  (Val_Sym $ sym_rpg^.attr.charDomains )
                    (Val_Set $ Set $ Set.fromList
                      [ Val_Sym $ sym_rpg^.domain.fire ] )
            , pair  (Val_Sym $ sym_rpg^.attr.books)
                    (Val_Arr $ Arr $ Seq.fromList [
                        Val_Text $ Text "The Human Cookbook"
                      , Val_Text $ Text "Treasure Hoarding"
                      , Val_Text $ Text "Learn to Speak Elven"
                      ]
                    )
            ]



