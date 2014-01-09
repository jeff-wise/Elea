

module Test.Data.RPG (
    dungeonP
  , cityP, castleP, mineP
  , bartenderP, priestP, beggarP
  , partySetP, partyArrP
  , heroP, warlockP, dragonP
  ) where



import Test.Prelude
import Test.Data.System

import Elea.Lang.Types


import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set



---------------------------------------------------------------------
-- RPG-Themed Particles
---------------------------------------------------------------------


-- Particle with simple Symbol ID
dungeonP = Particle
  { _partId   = Val_Sym $ sym_rpg^.entity.dungeon
  , _partVal  = Val_Sym $ sym_rpg^.entity.dungeon
  }




-- Particles with simple Text IDs
bartenderP = Particle
  { _partId   = Val_Text $ Text "Bartender"
  , _partVal  = Val_Text $ Text "Bartender"
  }

priestP = Particle
  { _partId   = Val_Text $ Text "Priest"
  , _partVal  = Val_Text $ Text "Priest"
  }

beggarP = Particle
  { _partId   = Val_Text $ Text "Beggar"
  , _partVal  = Val_Text $ Text "Beggar"
  }




-- Particles with simple Numeric IDs
cityP = Particle
  { _partId   = Val_Num $ Z 4
  , _partVal  = Val_Text $ Text "City"
  }

castleP = Particle
  { _partId   = Val_Num $ Z (-15)
  , _partVal  = Val_Text $ Text "Castle"
  }

mineP = Particle
  { _partId   = Val_Num $ R 30.3
  , _partVal  = Val_Text $ Text "Mine"
  }



-- Particle with Array ID
partyArrP = Particle
  { _partId   = Val_Arr $ Arr $ Seq.fromList [
                    Val_Sym $ sym_rpg^.entity.hero
                  , Val_Sym $ sym_rpg^.entity.warlock
                  , Val_Sym $ sym_rpg^.entity.rogue
                ]
  , _partVal  = Val_Text $ Text "Adventuring Party Array"
  }



-- Particle with Set ID
partySetP = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
                    Val_Sym $ sym_rpg^.entity.hero
                  , Val_Sym $ sym_rpg^.entity.warlock
                  , Val_Sym $ sym_rpg^.entity.rogue
                ]
  , _partVal  = Val_Text $ Text "Adventuring Party Set"
  }



-- Some particles with complex, deeply nested IDs
heroP = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
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
  , _partVal  = Val_Sym $ sym_rpg^.entity.hero
  }


warlockP = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
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
  , _partVal  = Val_Sym $ sym_rpg^.entity.warlock
  }



dragonP = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
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
  , _partVal  = Val_Sym $ sym_rpg^.entity.dragon
  }


