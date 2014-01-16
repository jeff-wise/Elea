

module Test.Elea.Lang.Index.Type (tests_TypeIndex) where


import Test.Prelude
import Test.Data.System
import Test.Data.RPG

import Elea.Lang.Atom.Types
import Elea.Lang.Index.Type


import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T




tests_TypeIndex =  testGroup "Type Index" [
      tests_symbolLookup
    , tests_textLookup
    , tests_numLookup
    , tests_setLookup
    , tests_arrayLookup
    , tests_complexLookup
  ]



-- Test Types
-------------------------------------------------

-- Symbol Types
ty_sym    = Ty_Sym $ IsSymbol $ sym_rpg^.entity.dragon
ty_anySym = Ty_Sym AnySymbol


-- Textual Types
ty_isText = Ty_Text $ IsText $ Text "Ocean"
ty_textLen = Ty_Text $ WithTextLen (Z 5)
ty_anyText = Ty_Text AnyText


-- Numeric Types
ty_isNum = Ty_Num $ IsNumber (R 4.5)

ty_gtNum1 = Ty_Num $ GreaterThan (Z 2)
ty_gtNum2 = Ty_Num $ GreaterThan (R 7.229)
ty_gtNum3 = Ty_Num $ GreaterThan (Z 10000)

ty_ltNum1 = Ty_Num $ LessThan (R (-32.4))
ty_ltNum2 = Ty_Num $ LessThan (R 7.0)
ty_ltNum3 = Ty_Num $ LessThan (Z 23)

ty_range1 = Ty_Num $ InRange (Z 0) (Z 50)
ty_range2 = Ty_Num $ InRange (R (-6.0)) (R 22.2)
ty_range3 = Ty_Num $ InRange (Z 0) (R 0.0000000001)

ty_even = Ty_Num Even
ty_odd = Ty_Num Odd
ty_int = Ty_Num Integer
ty_nonNeg = Ty_Num NonNegative
ty_numAny = Ty_Num AnyNumber


-- Set Types
ty_withElem1 = Ty_Set $ WithElem ty_isText
ty_withElem2 = Ty_Set $ WithElem ty_anyText
ty_withElem3 = Ty_Set $ WithElem ty_isNum
ty_withElem4 = Ty_Set $ WithElem ty_gtNum1

ty_setSize = Ty_Set $ SetWithSize (Z 2)
ty_anySet = Ty_Set AnySet


-- Array Types
ty_isArray1 =  Ty_Arr $ IsArray $ Seq.fromList [
                  Ty_Num $ IsNumber (Z 5)
                , Ty_Text $ IsText $ Text "Dog"
                , Ty_Num $ GreaterThan (R 3.3)
              ]
ty_isArray2 =  Ty_Arr $ IsArray $ Seq.fromList [
                  Ty_Num NonNegative
                , Ty_Text $ WithTextLen (Z 3)
                , Ty_Num $ LessThan (Z 100)
              ]

ty_withIndex1 = Ty_Arr $ WithIndex (Z 0)
                  (Ty_Num $ IsNumber (R 5.0))
ty_withIndex2 = Ty_Arr $ WithIndex (Z 2)
                  (Ty_Sym $ IsSymbol $ sym_rpg^.entity.dragon)
ty_withIndex3 = Ty_Arr $ WithIndex (Z 0)
                                   (Ty_Num Integer)

ty_anyArr = Ty_Arr AnyArray



-- Complex Types
ty_hasCharClass = Ty_Set $ WithElem $ fstTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.charClass)
ty_hasHealth = Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.health)
                    (Ty_Num $ LessThan $ Z 200)
ty_elvenBook = Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.books)
                    (Ty_Arr $ WithIndex (Z 2) 
                      (Ty_Text $ IsText $
                        Text "Learn to Speak Elven"))
ty_airDomain = Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.charDomains)
                    (Ty_Set $ WithElem $
                        Ty_Sym $ IsSymbol $ sym_rpg^.domain.air)


typeIndex ∷ TypeIndex T.Text
typeIndex =
 let insertTypes  = insert ty_sym           "sym"
                >>> insert ty_anySym        "anySym"
                >>> insert ty_isText        "isText"
                >>> insert ty_textLen       "textLen"
                >>> insert ty_anyText       "anyText"
                >>> insert ty_isNum         "isNum"
                >>> insert ty_gtNum1        "gtNum1"
                >>> insert ty_gtNum2        "gtNum2"
                >>> insert ty_gtNum3        "gtNum3"
                >>> insert ty_ltNum1        "ltNum1"
                >>> insert ty_ltNum2        "ltNum2"
                >>> insert ty_ltNum3        "ltNum3"
                >>> insert ty_range1        "range1"
                >>> insert ty_range2        "range2"
                >>> insert ty_range3        "range3"
                >>> insert ty_even          "even"
                >>> insert ty_odd           "odd"
                >>> insert ty_int           "int"
                >>> insert ty_nonNeg        "nonNeg"
                >>> insert ty_numAny        "numAny"
                >>> insert ty_withElem1     "withElem1"
                >>> insert ty_withElem2     "withElem2"
                >>> insert ty_withElem3     "withElem3"
                >>> insert ty_withElem4     "withElem4"
                >>> insert ty_setSize       "setSize"
                >>> insert ty_anySet        "anySet"
                >>> insert ty_isArray1      "isArray1"
                >>> insert ty_isArray2      "isArray2"
                >>> insert ty_withIndex1    "withIndex1"
                >>> insert ty_withIndex2    "withIndex2"
                >>> insert ty_withIndex3    "withIndex3"
                >>> insert ty_anyArr        "anyArr"
                >>> insert ty_hasCharClass  "hasCharClass"
                >>> insert ty_hasHealth     "hasHealth"
                >>> insert ty_elvenBook     "elvenBook"
                >>> insert ty_airDomain     "airDomain"
  in  insertTypes newTypeIndex



testLookup idx val = HS.fromList $ lookup val idx


tests_symbolLookup = testGroup "Symbol Lookup" [

    testCase "Lookup Dragon" $
        (testLookup typeIndex $ Val_Sym $ sym_rpg^.entity.dragon)
      @?=
        HS.fromList ["sym", "anySym"]
  

  , testCase "Lookup any symbol" $   
        (testLookup typeIndex $ Val_Sym $ sym_rpg^.entity.hero)
      @?=
        HS.fromList ["anySym"]
  ]




tests_textLookup = testGroup "Text Lookup" [
                                            
    testCase "Lookup 'Ocean'" $
        (testLookup typeIndex $ Val_Text $ Text "Ocean")
      @?=
        HS.fromList ["isText", "textLen", "anyText"]


  , testCase "Lookup any text" $
        (testLookup typeIndex $ Val_Text $ Text "Lion")
      @?=
        HS.fromList ["anyText"]
  ]




tests_numLookup = testGroup "Number Lookup" [
                                            
    testCase "Lookup 4.5" $
        (testLookup typeIndex $ Val_Num $ R 4.5)
      @?=
        HS.fromList [ "isNum", "gtNum1", "ltNum2", "ltNum3",
                      "range1", "range2", "nonNeg", "numAny" ]
                      
    
  , testCase "Lookup 25" $
        (testLookup typeIndex $ Val_Num $ Z 25)
      @?=
        HS.fromList [ "gtNum1", "gtNum2", "range1",
                      "odd", "int", "nonNeg", "numAny" ]

  , testCase "Lookup -6" $
        (testLookup typeIndex $ Val_Num $ Z (-6))
      @?=
        HS.fromList [ "ltNum2", "ltNum3", "range2",
                      "even", "int", "numAny" ]
                      
  
  , testCase "Lookup 0.00" $
        (testLookup typeIndex $ Val_Num $ R 0.00)
      @?=
        HS.fromList [ "ltNum2", "ltNum3", "range1", "range2",
                      "range3", "even", "int", "nonNeg", "numAny" ]


  , testCase "Lookup -59.7654321" $
        (testLookup typeIndex $ Val_Num $ R (-59.7654321))
      @?=
        HS.fromList [ "ltNum1", "ltNum2", "ltNum3", "numAny" ]
  ]




tests_setLookup = testGroup "Set Lookup" [

    testCase "Lookup { 'Ocean' } " $
        (testLookup typeIndex $
            Val_Set $ Set $ HS.singleton $
              Val_Text $ Text $ "Ocean" )
      @?=
        HS.fromList ["withElem1", "withElem2", "anySet"]


  , testCase "Lookup { 4.5 } " $
        (testLookup typeIndex $
            Val_Set $ Set $ HS.singleton $
              Val_Num $ R 4.5)
      @?=
        HS.fromList ["withElem3", "withElem4", "anySet"]


   , testCase "Lookup { 100, 'Earth' } " $
        (testLookup typeIndex $
            Val_Set $ Set $ HS.fromList [
                Val_Num $ Z 100
              , Val_Text $ Text "Earth"
            ]
        )
      @?=
        HS.fromList ["withElem2", "withElem4", "setSize", "anySet"]
  ]




tests_arrayLookup = testGroup "Array Lookup" [
                                             
    testCase "Lookup [5, 'Dog', 3,567.9] " $
        (testLookup typeIndex $
            Val_Arr $ Arr $ Seq.fromList [
                Val_Num $ Z 5
              , Val_Text $ Text "Dog"
              , Val_Num $ R 3567.9
            ]
        )
      @?=
        HS.fromList ["isArray1", "withIndex1", "withIndex3", "anyArr"]


  , testCase "Lookup [5, 'Dog', 50] " $
        (testLookup typeIndex $
            Val_Arr $ Arr $ Seq.fromList [
                Val_Num $ Z 5
              , Val_Text $ Text "Dog"
              , Val_Num $ Z 50
            ]
        )
      @?=
        HS.fromList [ "isArray1", "isArray2", "withIndex1",
                      "withIndex3", "anyArr"]


  , testCase "Lookup [0, 'Dinosaur', DRAGON] " $
        (testLookup typeIndex $
            Val_Arr $ Arr $ Seq.fromList [
                Val_Num $ Z 0
              , Val_Text $ Text "Dinosaur"
              , Val_Sym $ sym_rpg^.entity.dragon
            ]
        )
      @?=
        HS.fromList ["withIndex2", "withIndex3", "anyArr"]
  ]



{-
-- Complex Types
ty_hasCharClass = Ty_Set $ WithElem $ fstTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.charClass)
ty_hasHealth = Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.health)
                    (Ty_Num $ LessThan $ Z 200)
ty_elvenBook = Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.books)
                    (Ty_Arr $ WithIndex (Z 2) 
                      (Ty_Text $ IsText $
                        Text "Learn to Speak Elven"))
ty_airDomain = Ty_Set $ WithElem $ pairTy
                    (Ty_Sym $ IsSymbol $ sym_rpg^.attr.charDomains)
                    (Ty_Set $ WithElem $
                        Ty_Sym $ IsSymbol $ sym_rpg^.domain.air)
-}
tests_complexLookup = testGroup "Complex Lookup" [

    testCase "Lookup Hero" $
        testLookup typeIndex heroVal
      @?=
        HS.fromList ["hasCharClass", "hasHealth", "airDomain", "anySet"]
 
  , testCase "Lookup Warlock" $
        testLookup typeIndex warlockVal
      @?=
        HS.fromList ["hasCharClass", "airDomain", "anySet"]
 

  , testCase "Lookup Dragon" $
        testLookup typeIndex dragonVal
      @?=
        HS.fromList ["elvenBook", "anySet"]
  ]


