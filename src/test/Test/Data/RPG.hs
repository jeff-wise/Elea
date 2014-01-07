

module Test.Data.RPG (
    dungeonP
  , cityP
  , bartenderP
  , heroP
  , warlockP
  , dragonP
  ) where



import Test.Prelude
import Test.Data.System

import Elea.Lang.Types



import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set



---------------------------------------------------------------------
-- RPG-Themed Particles
---------------------------------------------------------------------


-- | Minimal particle defintion
dungeonP = Particle
  { _partId   = Val_Sym $ sym_rpg^.entity.dungeon
  , _partVal  = Val_Sym $ sym_rpg^.entity.dungeon
  }


cityP = Particle
  { _partId   = Val_Pair $ Pair (Val_Sym $ sym_rpg^.entity.city)
                                (Val_Text $ Text "Halatia")
  , _partVal  = Val_Sym $ sym_rpg^.entity.city
  }


bartenderP = Particle
  { _partId   = Val_Text $ Text "Bartender"
  , _partVal  = Val_Sym $ sym_rpg^.entity.bartender
  }


-- | The hero
heroP = Particle
  { _partId   = Val_Set $ Set $ Set.fromList [
                    Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.name)
                                    (Val_Text $ Text "Hero")
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.health)
                                    (Val_Num $ Z 100)
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.charClass)
                                    (Val_Text $ Text "Warrior" )
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.charDomains)
                                    (Val_Set $ Set $ Set.fromList [
                                        Val_Sym $ sym_rpg^.domain.fire
                                      , Val_Sym $ sym_rpg^.domain.air
                                      ]
                                    )
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.books)
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
                    Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.name)
                                    (Val_Text $ Text "Warlock")
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.health)
                                    (Val_Num $ Z 150)
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.charClass)
                                    (Val_Text $ Text "Warlock")
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.charDomains)
                                    (Val_Set $ Set $ Set.fromList [
                                        Val_Sym $ sym_rpg^.domain.earth
                                      , Val_Sym $ sym_rpg^.domain.fire
                                      , Val_Sym $ sym_rpg^.domain.death
                                      ]
                                    )
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.books)
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
                    Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.name)
                                    (Val_Text $ Text "Dragon")
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.health)
                                    (Val_Num $ Z 470)
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.charDomains )
                                    (Val_Set $ Set $ Set.fromList
                                      [ Val_Sym $ sym_rpg^.domain.fire ] )
                  , Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.books)
                                    (Val_Arr $ Arr $ Seq.fromList [
                                        Val_Text $ Text "The Human Cookbook"
                                      , Val_Text $ Text "Treasure Hoarding"
                                      , Val_Text $ Text "Learn to Speak Elven"
                                      ]
                                    )
                  ]
  , _partVal  = Val_Sym $ sym_rpg^.entity.dragon
  }


