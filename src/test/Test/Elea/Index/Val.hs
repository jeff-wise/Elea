

module Test.Elea.Index.Val (tests_ValIndex) where



import Test.Prelude
import Test.Data.RPG
import Test.Data.System

import Elea.Index.Val
import Elea.Lang.Types


import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq



-- TODO AND and OR

tests_ValIndex =  testGroup "Value Index" [
                    tests_symbolLookup
                  , tests_setLookup
                  , tests_arrayLookup
                  , tests_textLookup
                  , tests_numLookup
                  , tests_complexLookup
                  ]
 

-- flip lookup to make writing tests neater
-- and convert to set
testLookup valIndex ty = HS.fromList $ lookup ty valIndex




rpgValIndex = 
  let addRpgVals =  insert (_partId dungeonP  ) dungeonP
                >>> insert (_partId cityP     ) cityP
                >>> insert (_partId castleP   ) castleP
                >>> insert (_partId mineP     ) mineP
                >>> insert (_partId bartenderP) bartenderP
                >>> insert (_partId priestP   ) priestP
                >>> insert (_partId beggarP   ) beggarP
                >>> insert (_partId partySetP ) partySetP
                >>> insert (_partId partyArrP ) partyArrP
                >>> insert (_partId heroP     ) heroP
                >>> insert (_partId warlockP  ) warlockP
                >>> insert (_partId dragonP   ) dragonP
  in  addRpgVals newValIndex

 


tests_symbolLookup = testGroup "Symbol Lookup" [
  
  testCase "Only Dungeon identified by symbol Dungeon" $
        testLookup rpgValIndex
            (Ty_Sym $ IsSymbol $ sym_rpg^.entity.dungeon)
      @?=
        HS.fromList [dungeonP]
  ]
                               



tests_textLookup = testGroup "Text Lookup" [

    testCase "Only Priest identified by text Priest" $
        testLookup rpgValIndex  (Ty_Text $ IsText $ Text "Priest")
      @?=
        HS.fromList [priestP]

  , testCase "Both Priest and Beggar identified by text of length 6" $
        testLookup rpgValIndex  (Ty_Text $ WithTextLen (Z 6))
      @?=
        HS.fromList [priestP, beggarP]
  
  , testCase "Priest, Beggar, and Bartender have Textual IDs" $
        testLookup rpgValIndex (Ty_Text AnyText)
      @?=
        HS.fromList [bartenderP, priestP, beggarP]
  ]




tests_numLookup = testGroup "Number Lookup" [

    testCase "Only City has location EQUAL TO 4" $
        testLookup rpgValIndex (Ty_Num $ IsNumber (Z 4))
      @?=
        HS.fromList [cityP]
 

  , testCase "Only the City and Mine have location GREATER THAN 2.0" $
        testLookup rpgValIndex (Ty_Num $ GreaterThan (R 2.0))
      @?=
        HS.fromList [cityP, mineP]
    

  , testCase "Only the Castle has location LESS THAN 2.5" $
        testLookup rpgValIndex (Ty_Num $ LessThan (R 2.5))
      @?=
        HS.fromList [castleP]
  

  , testCase "Only the City has location IN RANGE of [4, 5.9]" $
        testLookup rpgValIndex (Ty_Num $ InRange (Z 4) (R 5.9))
      @?=
        HS.fromList [cityP]


  , testCase "Only the City has an EVEN location" $
        testLookup rpgValIndex (Ty_Num $ Even)
      @?=
        HS.fromList [cityP]


  , testCase "Only the Castle has an ODD location" $
        testLookup rpgValIndex (Ty_Num $ Odd)
      @?=
        HS.fromList [castleP]
  
  , testCase "Only Castle, City, and Mine have numeric IDs" $
        testLookup rpgValIndex (Ty_Num $ AnyNumber)
      @?=
        HS.fromList [castleP, cityP, mineP]
  ]




tests_setLookup = testGroup "Set Lookup" [

    testCase "Party has hero" $ 
        testLookup rpgValIndex 
            (Ty_Set $ WithElem $
                Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero)
      @?=
        HS.fromList [partySetP]

  , testCase "Party has 3 members" $ 
        testLookup rpgValIndex (Ty_Set $ SetWithSize $ Z 3)
      @?=
        HS.fromList [partySetP]

  , testCase "Party, Hero, Warlock, and Dragon are Sets" $
        testLookup rpgValIndex (Ty_Set AnySet)
      @?=
        HS.fromList [partySetP, heroP, warlockP, dragonP]

  ]




tests_arrayLookup = testGroup "Set Lookup" [

    testCase "First party member is Hero" $
        testLookup rpgValIndex 
            (Ty_Arr $ WithIndex (Z 0) 
                (Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero)
            )
      @?=
        HS.fromList [partyArrP]

  , testCase "Third party member is Rogue" $
        testLookup rpgValIndex 
            (Ty_Arr $
              WithIndex 
                (Z 2) 
                (Ty_Sym $ IsSymbol $ sym_rpg^.entity.rogue)
            )
      @?=
        HS.fromList [partyArrP]

  , testCase "Party has Hero, Warlock, and Rogue" $
        testLookup rpgValIndex
          (Ty_Arr $ IsArray $ Seq.fromList [
                Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero
              , Ty_Sym $ IsSymbol $ sym_rpg^.entity.warlock
              , Ty_Sym $ IsSymbol $ sym_rpg^.entity.rogue
            ]
          )
      @?=
        HS.fromList [partyArrP]


  , testCase "Party is an Array" $
        testLookup rpgValIndex (Ty_Arr AnyArray)
      @?=
        HS.fromList [partyArrP]
  ]




tests_complexLookup = testGroup "Complex Value Lookup" [ 

    testCase "Hero exists in index" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                    (Ty_Text $ IsText $ Text "Hero")
                )
      @?=
        HS.fromList [heroP]


  , testCase "Warlock exists in index" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                    (Ty_Text $ IsText $ Text "Warlock")
                )
      @?=
        HS.fromList [warlockP]


  , testCase "Dragon exists in index" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                    (Ty_Text $ IsText $ Text "Dragon")
                )
      @?=
        HS.fromList [dragonP]


  , testCase "Hero, Dragon, and Warlock have names" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ fstTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                )
      @?=
        HS.fromList [heroP, warlockP, dragonP]

  
  , testCase "Dragon and Warlock have more than 250 health" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.health)
                    (Ty_Num $ GreaterThan $ Z 250)
                )
      @?=
        HS.fromList [warlockP, dragonP]


  , testCase "2nd favorite book of Hero and Warlock is Legends and Lore" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.books)
                    (Ty_Arr $ WithIndex (Z 1) 
                        (Ty_Text $ IsText $ Text "Legends and Lore")
                    )
                )
      @?=
        HS.fromList [warlockP, heroP]


  , testCase "Hero and Dragon have Fire domain" $
        testLookup rpgValIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.charDomains)
                    (Ty_Set $ WithElem $
                      Ty_Sym $ IsSymbol $ sym_rpg^.domain.fire)
                )
      @?=
        HS.fromList [heroP, dragonP]

  ]



