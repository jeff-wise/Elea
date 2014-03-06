

-- | Kios
-- A game of 1 v 1 arena combat
module Test.App.Kios where

-- TODO how to name systems like classes


-- | Dictionary

-- | Types


ty_spell ∷ Type
ty_spell = Ty_Dict $ IsDict [
    ( "name"   , Ty_Text $ AnyText )
  , ( "domain" , Ty_Text $ AnyText )
  , ( "cost"   , Ty_Num $ AnyNumber)
  ]




-- WEAPONS
---------------------------------------
ty_weapon ∷ Type
ty_weapon = Ty_Or [ ty_weapon_dagger
                   , ty_weapon_spear ]

ty_weapon_dagger ∷ Type
ty_weapon_dagger = Ty_Dict $ IsDict [
    ( "name"
    , TextTy $ IsText "dagger" )
  , ( "damage"
    , NumberTy $ IsNumber (Z 1) )
  ]


ty_weapon_spear ∷ Type
ty_weapon_spear = Ty_Dict $ IsDict [
    ( "name"
    , TextTy $ IsText "dagger" )
  , ( "damage"
    , NumberTy $ InRange (Z 2) (Z 4) )
  ]


-- ARMOR
---------------------------------------
ty_armor ∷ Type
ty_armor = Ty_Or [ ty_armor_leather
                 , ty_armor_chainmail ]


ty_armor_leather ∷ Type
ty_armor_leather = Ty_Dict $ IsDict [
    ( "name"
    , TextTy $ IsText "Leather" )
    ( "resistance"
    , NumberTy $ IsNumber (Z 1) )
  ]


ty_armor_leather ∷ Type
ty_armor_leather = Ty_Dict $ IsDict [
    ( "name"
    , TextTy $ IsText "Chainmail" )
    ( "resistance"
    , NumberTy $ InRange (Z 3) (Z 6) )
  ]



-- SPELLS
---------------------------------------
ty_spell ∷ Type
ty_spell = Ty_Or [ ty_spell_melee, ty_spell_heal ]


ty_spell_melee ∷ Type
ty_spell_melee = Ty_Dict $ IsDict [
    ( "name", TextTy $ IsText "Melee" )
  , ( "params"
    , Ty_Dict $ IsDict [
        ( "attack", NumberTy $ AnyNumber )
      , ( "damage", NumberTy $ AnyNumber )
      ] )
  ]


ty_spell_heal ∷ Type
ty_spell_heal = Ty_Dict $ IsDict [
    ( "name"
    , TextTy $ IsText "Heal" )
  , ( "params"
    , Ty_Dict $ IsDict [
        ( "attack", NumberTy $ AnyNumber )
      , ( "damage", NumberTy $ AnyNumber )
      ] )
  ]



ty_char ∷ Type
ty_char = Ty_Dict $ IsDict [
    ( "name",   TextTy $ AnyText )
  , ( "weapon", ty_weapons)
  , ( "armor",  ty_armor  )
  , ( "spells", ty_spells )
  , ( "health", NumberTy $ IsNumber (Z 20))
  ]


-- ARENA
---------------------------------------
sysCons_arena ∷ SystemConstructor
sysCons_arena = SystemConstructor {
    _sysConSynth  = Synthesis $ HMS.fromList [
                      ( 1
                      , Application {
                          _appAbs = Abs_Const $ 
                            Val_Dict $ Dict $ HMS.fromList [
                              ("id", "arena") ]
                        , _appParams = []
                        }
                      ) 
                    ]
  , _sysConInter  = Nothing
  , _sysConConst  = Constraints {
                      constUnique = [
                        Lens (Lens_Dict $ AtKey "name" Lens_This)
                      ]
                    , constParts  = ty_char
                    }
}


create_arena ∷ Force
create_arena = Force_Create $ Create {
    _createSysLoc   = MainSys
  , _createSysCons  = sysCons_arena
  , _createSysInit  = Effect [
                        Group [
                          create_charCounter
                        , create_startMatch
                        ]
                      ]
  }




-- BATTLE
---------------------------------------


-- Spell Database
---------------------------------------
create_spellbook ∷ Force
create_spellbook = Force_Create $ Create
  { -- Create in the main applicaiton system
    _createLoc      = Absolute [
                        Val_Dict $ HasEntry
                          "id"
                          (Ty_Text $ IsText "kios")
                      ]
  , _createMemCons  = membCons_spellbook
  , _createSysInit  = initEffs_spellbook
  }



membCons_spellbook ∷ Cons_Membrane
membCons_spellbook = Cons_Membrane
  { _valueCons  = Cons_Value (synValueConst $
                    dict [ ("id", "spellbook") ]  
                  )
  , _interCons  = NoInteraction
  , _cnstrCons  = Cons_Constraints
                  { _consCnstrUnique  = [
                      synLensConst (Lens_Dict $ AtKey "name" Lens_This)
                    ]
                  , _consCnstrCells   = synTypeConst ty_spell
                  }
  }


-- COMBAT System
---------------------------------------

createCombatSystem ∷ Force
createCombatSystem = Force_Create $ Cons_System
  { _consSysLoc = Path_Absolute [
                        Val_Dict $ HasEntry
                          "id"
                          (Ty_Text $ IsText "kios")
                      ]
  , _consSysMem = combatMembrane
  , _consSysIni = [ Effect
                      { _effectPrec   = 3 
                      , _effectForce  = meleeBoson
                      }
                  , Effect 
                      { _effectPrec   = 3 
                      , _effectForce  = healBoson
                      }
                  , Effect 
                      { _effectPrec   = 3 
                      , _effectForce  = healBoson
                      }
                    ]
  }


combatMembrane ∷ Cons_Membrane
combatMembrane = Cons_Membrane {
    _valueCons  = Cons_Value (synValueConst $
                    dict [ ("id", "combat") ]  
                  )
  , _interCons  = NoInteraction
  , _cnstrCons  = Constraints {
                    constUnique = []
                  , constParts  = syn_ty_attack
                  }
  }



syn_ty_spells ∷ Type
syn_ty_spells = synthesis [
       -- Spell names
  , (1, Application {
          _appAbs     = Abs_Query $ Query
            { _qryFrom    = Path_Absolute [
                              Ty_Dict $ HasEntry
                                (Text "id")
                                (Ty_Text $ IsText "kios")
                            , Ty_Dict $ HasEntry
                                (Text "id")
                                (Ty_Text $ IsText "spellbook")
                            ]
            , _qryWhere   = Ty_Any
            , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
            }
        , _appParams  = []
        }
    )
  , (2 Application {
          _appAbs     = Abs_TypeTemp $ T_Ty_Text $ T_OneOfText $ Var 1
        , _appParams  = [1]
        }
    )
  ]


syn_ty_chars ∷ Synthesis
syn_ty_chars = synthesis [
     -- Character names
    (1, Application {
          _appAbs     = Abs_Query $ Query
            { _qryFrom    = Path_Absolute [
                              Ty_Dict $ HasEntry
                                (Text "id")
                                (Ty_Text $ IsText "kios")
                            , Ty_Dict $ HasEntry
                                (Text "id")
                                (Ty_Text $ IsText "arena")
                            ]
            , _qryWhere   = Ty_Any
            , _qrySelect  = Lens_Dict $ AtKey "name" Lens_This
            }
        , _appParams  = []
        }
    )
  , (2, Application {
          _appAbs     = Abs_TypeTemp $ T_Ty_Text $ T_OneOfText $ Var 1
        , _appParams  = [1]
        }
    )
  ]



syn_ty_attack ∷ Synthesis
syn_ty_attack = synthesis [
    (1, Abs_SynType syn_ty_chars)
  , (2, Abs_SynType syn_ty_spells)
  , (3, Abs_TypeTemp $ T_Ty_Dict $ T_IsDict [
          ( Dec $ T_Text "attacker"
          , Var 1 
          )
        , ( Dec $ T_Text "spell"
          , Var 2
          )
        , ( Dec $ T_Text "type"
          , T_Ty_Text $ T_IsText "attack"
          )
        ]
    )
  ]




-- Spell Bosons
---------------------------------------
create_boson_spell ∷ Cons_Membrane → Force
create_boson_spell memCons = Force_Create $ Create
  { _createLoc      = Path_Absolute [
                        Val_Dict $ HasEntry
                          "id"
                          (Ty_Text $ IsText "kios")
                      , Ty_Dict $ HasEntry
                          (Text "id")
                          (Ty_Text $ IsText "battle")
                      ]
  , _createMemCons  = membCons
  , _createSysInit  = noEffect
  }



membCons_spell_melee ∷ Cons_Membrane
membCons_spell_melee = Cons_Membrane
  { _valueCons  = Cons_Value (synValueConst $
                    dict [ ("id", "melee") ]  
                  )
  , _interCons  = Cons_Interaction
      { _consInterCause = (synTypeConst $ Ty_Dict $ 
                            HasEntry "type" (Ty_Text $ IsText "attack"))
      , _consInterEffects = [ Effect
                                { _effectPrec   = 3 
                                , _effectForce  = attackerAfterMelee
                                }
                            , Effect
                                { _effectPrec   = 3 
                                , _effectForce  = defenderAfterMelee
                                }
                            ]
      }
  , _cnstrCons  = bosonConstraints
  }



attackerAfterMelee ∷ Force
attackerAfterMelee = Force_Update $ Cons_System
  { _createLoc      = []
  , _createMemCons  = membCons_melee
  , _createEffect   = Nothing
  }



-- BATTLE System
---------------------------------------
create_battle ∷ Force
create_battle = Force_Create $ Create
  { _createLoc      = Path_Absolute [
                        Val_Dict $ HasEntry
                          "id"
                          (Ty_Text $ IsText "kios")
                      ]
  , _createMemCons  = membCons_battle
  , _createSysInit  = Effect [ Group [ create_turn ] ]
  }



membCons_battle ∷ Cons_Membrane
membCons_battle = Cons_Membrane {
    _valueCons  = Cons_Value (synValueConst $
                    dict [ ("id", "battle") ]  
                  )
  , _interCons  = NoInteraction
  , _cnstrCons  = Constraints {
                              -- TODO make attak/defn unique
                    constUnique = []
                  , constParts  = ty_turn
                  }
  }


syn_ty_turn ∷ Synthesis
syn_ty_turn = synthesis [
    (1, Abs_SynType syn_ty_chars)
  , (2, Abs_TypeTemp $ T_Ty_Dict $ T_IsDict [
          ( Dec $ T_Text "attacker"
          , Var 1 
          )
        , ( Dec $ T_Text "defender"
          , Var 1
          )
        ]
    )
  ]

