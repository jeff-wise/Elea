

--
--         {{@}}
--          |@|
--          |@|
--          )8(
--    (@)__/8@8\__(@)
--      `'-=):(=-'`
--          |.|
--          |.|
--          |'|       _    _           
--          |.|      | | _(_) ___  ___ 
--          |'|      | |/ / |/ _ \/ __|
--          |.|      |   <| | (_) \__ \
--          |'|      |_|\_\_|\___/|___/
--          |.|
--          |'|
--          \_/
--           
--  
module Test.App.Kios where



import Elea.Lang.Term.System as Sys
import Elea.Lang.Term.Synthesis as Syn



---------------------------------------------------------------------
-- ******************************************************************
--                          Kios Library
-- ******************************************************************
---------------------------------------------------------------------


---------------------------------------------------------------------
-- (1) Systems
---------------------------------------------------------------------


-- kios (Kios)
---------------------------------------------------------------------

kios ∷ Cons_System
kios =
  Cons_System
    "/"
    (Cons_Membrane val_kios Nothing cnstrs_kios)


-- kios.arena (Arena)
---------------------------------------------------------------------

arena ∷ Cons_System
arena =
  Cons_System
    "/kios"
    (Cons_Membrane val_arena Nothing cnstrs_char)



-- kios.arena.btlStr (Battle Starter)
---------------------------------------------------------------------

battleStarter ∷ Cons_System
battleStarter =
  Cons_System
    "/kios/arena"
    (Cons_Membrane val_battleStarter startBattle Sys.particle)



-- | kios.battle (Battle)
-- A battle is just a series of turns which ends
-- when one of the characters is defeated.
---------------------------------------------------------------------

battle ∷ Cons_System
battle =
  Cons_System
    "/kios"
    (Cons_Membrane val_battle Nothing cnstrs_battle)



-- | kios.battle.turn (Turn)
-- A turn is a move by a single character. Characters control
-- alternating turns to choose attacks.
---------------------------------------------------------------------

turn ∷ Cons_System
turn =
  Cons_System
    "/kios/battle"
    (Cons_Membrane val_turn Nothing Sys.particle)



-- kios.spell_db (Spell Database)
---------------------------------------------------------------------

spellDB ∷ Cons_System
spellDB =
  Cons_System
    (URITy)
    (Cons_Membrane val_spellDB Nothing cnstrs_spell)


-- kios.spell_db.melee (Melee)
---------------------------------------------------------------------

spell_melee ∷ Cons_System
spell_melee =
  Cons_System
    (URITy)
    (Cons_Membrane val_melee Nothing Sys.particle)


-- kios.spell_db.heal (Heal)
---------------------------------------------------------------------

spell_heal ∷ Cons_System
spell_heal =
  Cons_System
    (URITy)
    (Cons_Membrane val_heal Nothing Sys.particle)


-- kios.weapon_db (Weapon Database)
---------------------------------------------------------------------

weaponDB ∷ Cons_System
weaponDB =
  Cons_System
    (URITy)
    (Cons_Membrane val_spellDB Nothing cnstrs_weapon)


-- kios.weapon_db.dagger (Dagger)
---------------------------------------------------------------------

wpn_dagger ∷ Cons_System
wpn_dagger =
  Cons_System
    (URITy)
    (Cons_Membrane val_dagger Nothing Sys.particle)


-- kios.weapon_db.spear (Spear)
---------------------------------------------------------------------

wpn_spear ∷ Cons_System
wpn_spear =
  Cons_System
    (URITy)
    (Cons_Membrane val_spear Nothing Sys.particle)



-- kios.armor_db (Armor Database)
---------------------------------------------------------------------

armorDB ∷ Cons_System
armorDB =
  Cons_System
    (URITy)
    (Cons_Membrane val_armorDB Nothing cnstrs_armor)


-- kios.armor_db.leather (Leather Armor)
---------------------------------------------------------------------

armor_leather ∷ Cons_System
armor_leather =
  Cons_System
    (URITy)
    (Cons_Membrane val_leather Nothing Sys.particle)


-- kios.armor_db.chainmail (Chainmail Armor)
---------------------------------------------------------------------

armor_chainmail ∷ Cons_System
armor_chainmail =
  Cons_System
    (URITy)
    (Cons_Membrane val_chainmail Nothing Sys.particle)




---------------------------------------------------------------------
-- (2) Constraints
---------------------------------------------------------------------

cnstrs_kios ∷ Cons_Constraints
cnstrs_kios = 
  Cons_Constraints [] ty_kios


cnstrs_char ∷ Cons_Constraints
cnstrs_char = 
  Cons_Constraints 
    (UniqConstraint [Lens_Dict $ AtKey "name" Lens_This])
    (ty_char)
  


cnstrs_spell ∷ Cons_Constraints
cnstrs_spell = 
  Cons_Constraints
    (UniqConstraint [Lens_Dict $ AtKey "name" Lens_This])
    (ty_spell)



cnstrs_weapon ∷ Cons_Constraints
cnstrs_weapon = 
  Cons_Constraints
    (UniqConstraint [Lens_Dict $ AtKey "name" Lens_This])
    (ty_weapon)



cnstrs_armor ∷ Cons_Constraints
cnstrs_armor = 
  Cons_Constraints
    ([UniqConstraint [Lens_Dict $ AtKey "name" Lens_This]])
    (ty_armor)



cnstrs_battle ∷ Cons_Constraints
cnstrs_battle = Cons_Constraints [] ty_turn


---------------------------------------------------------------------
-- (4) Values
---------------------------------------------------------------------


-- kios
---------------------------------------------------------------------

val_kios ∷ Synthesis
val_kios = Syn.valConst $ dict [("id", text "kios")]


-- kios.arena
---------------------------------------------------------------------

val_arena ∷ Synthesis
val_arena = Syn.valConst $ dict [("id", text "arena")]


-- kios.arena.btlstr
---------------------------------------------------------------------

val_battleStarter ∷ Synthesis
val_battleStarter = Syn.valConst $ dict [("id", text "battle_starter")]



-- kios.battle
---------------------------------------------------------------------
val_battle ∷ Synthesis
val_battle = Syn.valConst $ dict [("id", text "battle")]




-- kios.spell_db.melee
---------------------------------------------------------------------

val_melee ∷ Synthesis
val_melee = Syn.valConst $ dict [
    ( "name"    , text "melee"   )
  , ( "domain"  , text "physical")
  , ( "base-cost", int 0          )
  ]


-- kios.spell_db.heal
---------------------------------------------------------------------

val_heal ∷ Synthesis
val_heal = Syn.valConst $ dict [
    ( "name"    , text "heal"     )
  , ( "domain"  , text "spiritual")
  , ( "base-cost", int 0           )
  ]


-- kios.weapon_db.dagger
---------------------------------------------------------------------

val_dagger ∷ Synthesis
val_dagger = Syn.valConst $ dict [
    ( "name"      , text "dagger")
  , ( "base-damage", int 1       )
  ]


-- kios.weapon_db.spear
---------------------------------------------------------------------

val_spear ∷ Synthesis
val_spear = Syn.valConst $ dict [
    ( "name"       , text "spear")
  , ( "base-damage", int 3       )
  ]


-- kios.armor_db.leather
---------------------------------------------------------------------

val_leather ∷ Synthesis
val_leather = Syn.valConst $ dict [
    ( "name"           , text "leather")
  , ( "base-resistance", int 1         )
  ]


-- kios.armor_db.chainmail
---------------------------------------------------------------------

val_chainmail ∷ Synthesis
val_chainmail = Syn.valConst $ dict [
    ( "name"           , text "chainmail")
  , ( "base-resistance", int 3           )
  ]



-- | kios.battle.turn
-- A turn is randomly assigned an attacker and a defender
-- from among the players in the arena.
-- The probability of those assignments is determined by a
-- number of factors.
---------------------------------------------------------------------

val_turn ∷ Synthesis
val_turn = synthesis [
    (1, Application
          ( Abs_Query $ Query
              { _qryFrom    = "/kios/arena"
              , _qryWhere   = Ty_Any
              , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
              }
          )
          ([])
    )
  , (2, Application
          ( Abs_Random $
              -- TODO prob dist value?
              MapRandom
                (1)
                ( Abs_ValTemp $ T_Val_Dict $ T_Dict $ HMS.fromList [
                    (Dec $ T_Text "attacker", Var 1)
                  , (Dec $ T_Text "defender", Var 2)
                  ]
                )
          ([1])
  ]



-- on turn create
-- send request to player
-- wait for attack in combat system
-- send update to arena & new turn (lower priority)




---------------------------------------------------------------------
-- (5) Types
---------------------------------------------------------------------


-- kios
---------------------------------------------------------------------

ty_kios ∷ Synthesis
ty_kios = Syn.tyConst $ TyOr [
    isDict [ ("id", "arena")]
  , isDict [ ("id", "battle")]
  , isDict [ ("id", "combat")]
  , isDict [ ("id", "spell_db")]
  , isDict [ ("id", "weapon_db")]
  , isDict [ ("id", "armor_db")]
  ]


-- kios.arena.char
---------------------------------------------------------------------

ty_char ∷ Synthesis
ty_char = Syn.tyConst $ isDict [
    ( "name"  , TextTy AnyText            )
  , ( "weapon", ty_isWeaponName           )
  , ( "armor" , ty_isArmorName            )
  , ( "spells", ty_isSpellName            )
  , ( "health", NumberTy $ IsNumber (Z 20))
  ]


-- kios.spell_db.spell
---------------------------------------------------------------------

ty_spell ∷ Synthesis
ty_spell = Syn.tyConst $ isDict [
    ( "name"   , Ty_Text AnyText )
  , ( "domain" , Ty_Text AnyText )
  , ( "cost"   , Ty_Num NonNegative )
  ]


-- kios.battle.turn
---------------------------------------------------------------------

ty_turn ∷ Synthesis
ty_turn = synthesis [
    (1, Abs_SynType ty_isCharName)
  , (2, Abs_TypeTemp $ T_Ty_Dict $ T_IsDict [
          ( Dec $ T_Text "attacker" , Var 1 )
        , ( Dec $ T_Text "defender" , Var 1 )
        --, ( Dec $ T_Text "situation", 
        ]
    )
  ]



-- kios.armor_db.armor
---------------------------------------------------------------------

ty_armor ∷ Synthesis
ty_armor = Syn.tyConst $ isDict [
    ( "name"      , Ty_Text AnyText   )
  , ( "resistance", Ty_Num NonNegative)
  ]


-- kios.weapon_db.weapon
---------------------------------------------------------------------

ty_weapon ∷ Synthesis
ty_weapon = Syn.tyConst $ isDict [
    ( "name"  , Ty_Text AnyText   )
  , ( "damage", Ty_Num NonNegative)
  ]



-- kios.is_spell_name
---------------------------------------------------------------------

ty_isSpellName ∷ Type
ty_isSpellName = synthesis [
       -- Spell names
  , (1, Application
          ( Abs_Query $ Query
              { _qryFrom    = "/kios/spellDB"
              , _qryWhere   = Ty_Any
              , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
              }
          )
          ( [] )
    )
  , (2 Application
        (Abs_TypeTemp $ T_Ty_Text $ T_OneOfText $ Var 1)
        ([1])
    )
  ]


-- kios.is_char_name
---------------------------------------------------------------------

ty_isCharName ∷ Synthesis
ty_isCharName = synthesis [
     -- Character names
    (1, Application
          ( Abs_Query $ Query
              { _qryFrom    = "/kios/arena"
              , _qryWhere   = Ty_Any
              , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
              }
          )
          ( [] )
    )
  , (2, Application
          (Abs_TypeTemp $ T_Ty_Text $ T_OneOfText $ Var 1)
          ([1])
    )
  ]



-- kios.is_weapon_name
---------------------------------------------------------------------

ty_isWeaponName ∷ Synthesis
ty_isWeaponName = synthesis [
     -- Character names
    (1, Application
          ( Abs_Query $ Query
              { _qryFrom    = "/kios/weaponDB"
              , _qryWhere   = Ty_Any
              , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
              }
          )
          ( [] )
    )
  , (2, Application
          (Abs_TypeTemp $ T_Ty_Text $ T_OneOfText $ Var 1)
          ([1])
    )
  ]



-- kios.is_armor_name
---------------------------------------------------------------------

ty_isArmorName ∷ Synthesis
ty_isArmorName = synthesis [
     -- Character names
    (1, Application
          ( Abs_Query $ Query
              { _qryFrom    = "/kios/armorDB"
              , _qryWhere   = Ty_Any
              , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
              }
          )
          ( [] )
    )
  , (2, Application
          (Abs_TypeTemp $ T_Ty_Text $ T_OneOfText $ Var 1)
          ([1])
    )
  ]


-- | kios.combat.attack
---------------------------------------------------------------------

ty_attack ∷ Synthesis
ty_attack = synthesis [
    (1, Application 
          (Abs_SynType ty_isSpellName)
          ([])
  , (2, Application
          (Abs_TypeTemp $ T_Ty_Dict $ T_IsDict [
              ( Dec $ T_Text "spell", Var 1)
            ]
          )
          ([1])
    )
  ]



---------------------------------------------------------------------
-- (5) Interactions
---------------------------------------------------------------------

startBattle ∷ Cons_Interaction
startBattle =
  Cons_Interaction
    (Cons_OnThreshold (Syn.valConst $ int 2) (Syn.tyConst Ty_Any))
    (newTurn)



---------------------------------------------------------------------
-- (5) Effect Queues
---------------------------------------------------------------------

newTurn ∷ EffectQueue
newTurn = effectQueue [ Effect 0 (Force_Create turn)]




