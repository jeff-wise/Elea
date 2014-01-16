

module Test.Elea.Lang.Index.Val (tests_ValIndex) where



import Test.Prelude
import Test.Data.RPG
import Test.Data.System


import Elea.Lang.Index.Val
import Elea.Lang.Atom.Types


import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T



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
testLookup idx ty = HS.fromList $ lookup ty idx



valIndex ∷ ValIndex T.Text
valIndex = 
  let addRpgVals =  insert dungeonVal    "Dungeon"
                >>> insert cityVal       "City"
                >>> insert castleVal     "Castle"
                >>> insert mineVal       "Mine"
                >>> insert bartenderVal  "Bartender"
                >>> insert priestVal     "Priest"
                >>> insert beggarVal     "Beggar"
                >>> insert partySetVal   "PartySet"
                >>> insert partyArrVal   "PartyArr"
                >>> insert heroVal       "Hero"
                >>> insert warlockVal    "Warlock"
                >>> insert dragonVal     "Dragon"
  in  addRpgVals newValIndex

 


tests_symbolLookup = testGroup "Symbol Lookup" [
  
  testCase "Only Dungeon identified by symbol Dungeon" $
        testLookup valIndex
            (Ty_Sym $ IsSymbol $ sym_rpg^.entity.dungeon)
      @?=
        HS.fromList ["Dungeon"]
  ]
                               



tests_textLookup = testGroup "Text Lookup" [

    testCase "Only Priest identified by text Priest" $
        testLookup valIndex  (Ty_Text $ IsText $ Text "Priest")
      @?=
        HS.fromList ["Priest"]

  , testCase "Both Priest and Beggar identified by text of length 6" $
        testLookup valIndex  (Ty_Text $ WithTextLen (Z 6))
      @?=
        HS.fromList ["Priest", "Beggar"]
  
  , testCase "Priest, Beggar, and Bartender have Textual IDs" $
        testLookup valIndex (Ty_Text AnyText)
      @?=
        HS.fromList ["Bartender", "Priest", "Beggar"]
  ]




tests_numLookup = testGroup "Number Lookup" [

    testCase "Only City has location EQUAL TO 4" $
        testLookup valIndex (Ty_Num $ IsNumber (Z 4))
      @?=
        HS.fromList ["City"]
 

  , testCase "Only the City and Mine have location GREATER THAN 2.0" $
        testLookup valIndex (Ty_Num $ GreaterThan (R 2.0))
      @?=
        HS.fromList ["City", "Mine"]
    

  , testCase "Only the Castle has location LESS THAN 2.5" $
        testLookup valIndex (Ty_Num $ LessThan (R 2.5))
      @?=
        HS.fromList ["Castle"]
  

  , testCase "Only the City has location IN RANGE of [4, 5.9]" $
        testLookup valIndex (Ty_Num $ InRange (Z 4) (R 5.9))
      @?=
        HS.fromList ["City"]


  , testCase "Only the City has an EVEN location" $
        testLookup valIndex (Ty_Num $ Even)
      @?=
        HS.fromList ["City"]


  , testCase "Only the Castle has an ODD location" $
        testLookup valIndex (Ty_Num $ Odd)
      @?=
        HS.fromList ["Castle"]
  
  , testCase "Only Castle, City, and Mine have numeric IDs" $
        testLookup valIndex (Ty_Num $ AnyNumber)
      @?=
        HS.fromList ["Castle", "City", "Mine"]
  ]




tests_setLookup = testGroup "Set Lookup" [

    testCase "Party has hero" $ 
        testLookup valIndex 
            (Ty_Set $ WithElem $
                Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero)
      @?=
        HS.fromList ["PartySet"]

  , testCase "Party has 3 members" $ 
        testLookup valIndex (Ty_Set $ SetWithSize $ Z 3)
      @?=
        HS.fromList ["PartySet"]

  , testCase "Party, Hero, Warlock, and Dragon are Sets" $
        testLookup valIndex (Ty_Set AnySet)
      @?=
        HS.fromList ["PartySet", "Hero", "Warlock", "Dragon"]

  , testCase "Party contains exactly Hero, Warlock, and Rogue" $
        testLookup valIndex (Ty_Set $ IsSet $ HS.fromList [
              Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero
            , Ty_Sym $ IsSymbol $ sym_rpg^.entity.warlock
            , Ty_Sym $ IsSymbol $ sym_rpg^.entity.rogue
            ]
          )
      @?=
        HS.fromList ["PartySet"]
  ]




tests_arrayLookup = testGroup "Set Lookup" [

    testCase "First party member is Hero" $
        testLookup valIndex 
            (Ty_Arr $ WithIndex (Z 0) 
                (Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero)
            )
      @?=
        HS.fromList ["PartyArr"]

  , testCase "Third party member is Rogue" $
        testLookup valIndex 
            (Ty_Arr $
              WithIndex 
                (Z 2) 
                (Ty_Sym $ IsSymbol $ sym_rpg^.entity.rogue)
            )
      @?=
        HS.fromList ["PartyArr"]

  , testCase "Party has Hero, Warlock, and Rogue" $
        testLookup valIndex
          (Ty_Arr $ IsArray $ Seq.fromList [
                Ty_Sym $ IsSymbol $ sym_rpg^.entity.hero
              , Ty_Sym $ IsSymbol $ sym_rpg^.entity.warlock
              , Ty_Sym $ IsSymbol $ sym_rpg^.entity.rogue
            ]
          )
      @?=
        HS.fromList ["PartyArr"]


  , testCase "Party is an Array" $
        testLookup valIndex (Ty_Arr AnyArray)
      @?=
        HS.fromList ["PartyArr"]
  ]




tests_complexLookup = testGroup "Complex Value Lookup" [ 

    testCase "Hero exists in index" $
        testLookup valIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                    (Ty_Text $ IsText $ Text "Hero")
                )
      @?=
        HS.fromList ["Hero"]


  , testCase "Warlock exists in index" $
        testLookup valIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                    (Ty_Text $ IsText $ Text "Warlock")
                )
      @?=
        HS.fromList ["Warlock"]


  , testCase "Dragon exists in index" $
        testLookup valIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                    (Ty_Text $ IsText $ Text "Dragon")
                )
      @?=
        HS.fromList ["Dragon"]


  , testCase "Hero, Dragon, and Warlock have names" $
        testLookup valIndex
                (Ty_Set $ WithElem $ fstTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.name)
                )
      @?=
        HS.fromList ["Hero", "Warlock", "Dragon"]

  
  , testCase "Dragon and Warlock have more than 250 health" $
        testLookup valIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.health)
                    (Ty_Num $ GreaterThan $ Z 250)
                )
      @?=
        HS.fromList ["Warlock", "Dragon"]


  , testCase "2nd favorite book of Hero and Warlock is Legends and Lore" $
        testLookup valIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.books)
                    (Ty_Arr $ WithIndex (Z 1) 
                        (Ty_Text $ IsText $ Text "Legends and Lore")
                    )
                )
      @?=
        HS.fromList ["Warlock", "Hero"]


  , testCase "Hero and Dragon have Fire domain" $
        testLookup valIndex
                (Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.charDomains)
                    (Ty_Set $ WithElem $
                      Ty_Sym $ IsSymbol $ sym_rpg^.domain.fire)
                )
      @?=
        HS.fromList ["Hero", "Dragon"]

  ]



