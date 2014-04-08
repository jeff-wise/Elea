

-- | Type Index Tests
---
-- Create a TypeIndex with some sensible default types.
--
-- Ignore the internal representation. 
-- 
-- Test that it works by performing lookups for various
-- values. If all the lookups are successful, by returning
-- all of the types which match a given value, then the
-- index is semantically correct.
--
-- Account for edge cases as much as possible, but right now
-- the type/value specification is still subject to change,
-- so most tests will only cover basic functionality.
module EleaTest.Lang.Sem.TypeIndex (tests_TypeIndex) where


import EleaTest.Prelude

import Elea.Lang.Term.Value
import Elea.Lang.Term.Type
import Elea.Lang.Sem.TypeIndex


import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq




---------------------- TYPE INDEX TESTS --------------------

tests_TypeIndex =  testGroup "Type Index"
  [ tests_recordLookup
  , tests_arrayLookup
  , tests_textLookup
  , tests_numberLookup
  ]




------------------------ TEST TYPES ------------------------

-- ***** Record Types

hasAgeEntry   = Ty_Rec $ HasEntry "age" (Ty_Num AnyNumber)

hasNameEntry  = Ty_Rec $ HasEntry "name" (Ty_Txt AnyText)

hasSizeTwo    = Ty_Rec $ WithSize 2

anyRecord     = Ty_Rec AnyRecord



-- ***** Array Types

hasTextAtTwo  = Ty_Arr $ WithIndex 2 (Ty_Txt AnyText)

hasPosAtZero  = Ty_Arr $ WithIndex 0
                    (Ty_Num $ GreaterThan $ Z 0)

arrWithLen3   = Ty_Arr $ WithArrLen 3

anyArray      = Ty_Arr AnyArray



-- ***** And Types

simpleRecord = Ty_And $ AndType
  [ Ty_Rec $ HasEntry "age" $ Ty_Num $ GreaterThan (Z 18)
  , Ty_Rec $ HasEntry "name" $ Ty_Txt AnyText
  ]

simpleArray = Ty_And $ AndType
  [ Ty_Arr $ WithIndex 0 $ Ty_Num $ LessThan (R 65.4)
  , Ty_Arr $ WithArrLen 3
  ]

textLen4andBoat = Ty_And $ AndType
  [ Ty_Txt $ IsText $ Text "boat"
  , Ty_Txt $ WithTextLen 4
  ]



-- ***** Or Types

recOrArr = Ty_Or $ OrType [Ty_Rec AnyRecord, Ty_Arr AnyArray] 

lt0OrGt10 = Ty_Or $ OrType
  [ Ty_Num $ LessThan $ Z 0 
  , Ty_Num $ GreaterThan $ Z 10
  ]

dogOrCat = Ty_Or $ OrType
  [ Ty_Txt $ IsText $ Text "dog"
  , Ty_Txt $ IsText $ Text "cat"
  ]



-- ***** Textual Types

isOceanText    = Ty_Txt $ IsText $ Text "Ocean"
textLengthFive = Ty_Txt $ WithTextLen 5
anyText        = Ty_Txt AnyText



-- ***** Numeric Types

isNum = Ty_Num $ IsNumber (R 4.5)

gtNum1 = Ty_Num $ GreaterThan (Z 2)
gtNum2 = Ty_Num $ GreaterThan (R 717.229)

ltNum1 = Ty_Num $ LessThan (R (-32.4))
ltNum2 = Ty_Num $ LessThan (Z 70)

range1 = Ty_Num $ Range (Z 0) (Z 50)
range2 = Ty_Num $ Range (R (-6.0)) (R 22.2)
range3 = Ty_Num $ Range (Z 0) (R 0.0000000001)

anyNumber = Ty_Num AnyNumber


-- ***** Any Type
anyType = Ty_Any




---------------------- TEST TYPE INDEX ---------------------

testTypeIndex ∷ TypeIndex
testTypeIndex =
  let insertTypes = insert hasAgeEntry
                >>> insert hasNameEntry
                >>> insert hasSizeTwo
                >>> insert anyRecord
                >>> insert hasTextAtTwo
                >>> insert hasPosAtZero
                >>> insert arrWithLen3
                >>> insert anyArray
                >>> insert simpleRecord
                >>> insert simpleArray
                >>> insert textLen4andBoat
                >>> insert recOrArr
                >>> insert lt0OrGt10
                >>> insert dogOrCat
                >>> insert isOceanText
                >>> insert textLengthFive
                >>> insert anyText
                >>> insert isNum
                >>> insert gtNum1
                >>> insert gtNum2
                >>> insert ltNum1
                >>> insert ltNum2
                >>> insert range1
                >>> insert range2
                >>> insert range3
                >>> insert anyNumber
                >>> insert anyType
  in  insertTypes newTypeIndex




----------------------- RECORD LOOKUP ----------------------


tests_recordLookup = testGroup "Record Lookup"
  [ test1_recordLookup
  , test2_recordLookup
  ]                                          
  


test1_recordLookup =
  let nameRecord =  Val_Rec $ Rec $
                      HMS.singleton "name" $
                        Val_Txt $ Text "Bob"
  in  testCase "Lookup Name Record" $
        lookup nameRecord testTypeIndex
          @?=
        HS.fromList [anyType, recOrArr, hasNameEntry, anyRecord]
  

test2_recordLookup =
  let personRecord =  Val_Rec $ Rec $ HMS.fromList
                        [ ("name", Val_Txt $ Text "James")
                        , ("age", Val_Num $ Z 25)
                        ]
  in  testCase "Lookup Person Record" $
        lookup personRecord testTypeIndex
          @?=
        HS.fromList [recOrArr, hasNameEntry, hasAgeEntry,
                     hasSizeTwo, anyRecord, simpleRecord, anyType]
  



------------------------ ARRAY LOOKUP ----------------------

tests_arrayLookup = testGroup "Array Lookup"
  [ test1_arrayLookup
  , test2_arrayLookup
  ]                                          
  


test1_arrayLookup =
  let arrayWith10 = Val_Arr $ Arr $ Seq.fromList [ Val_Num $ Z 10 ]
  in  testCase "Lookup [10]" $
        lookup arrayWith10 testTypeIndex
          @?=
        HS.fromList [anyType, anyArray, hasPosAtZero, recOrArr]


test2_arrayLookup =
  let randomArray = Val_Arr $ Arr $ Seq.fromList
                      [ Val_Num $ Z (-10)
                      , Val_Txt $ Text "book"
                      , Val_Txt $ Text "dinosaur"
                      ]
  in  testCase "Lookup [-10, book, dinosaur]" $
        lookup randomArray testTypeIndex
          @?=
        HS.fromList [ anyArray, recOrArr, hasTextAtTwo
                    , arrWithLen3, anyType, simpleArray, anyType]




------------------------- TEXT LOOKUP ----------------------

tests_textLookup = testGroup "Text Lookup"
  [ test1_textLookup
  , test2_textLookup
  , test3_textLookup
  ]                                          
  


test1_textLookup =
  let oceanText = Val_Txt $ Text "Ocean"
  in  testCase "Lookup 'Ocean'" $
        lookup oceanText testTypeIndex
          @?=
        HS.fromList [anyText, isOceanText, anyType, textLengthFive]


test2_textLookup =
  let dogText = Val_Txt $ Text "dog"
  in  testCase "Lookup 'dog" $
        lookup dogText testTypeIndex
          @?=
        HS.fromList [anyType, anyText, dogOrCat]


test3_textLookup =
  let boatText = Val_Txt $ Text "boat"
  in  testCase "Lookup 'boat" $
        lookup boatText testTypeIndex
          @?=
        HS.fromList [anyType, anyText, textLen4andBoat]




------------------------- NUMBER LOOKUP ----------------------

tests_numberLookup = testGroup "Number Lookup"
  [ test1_numberLookup
  , test2_numberLookup                                          
  , test3_numberLookup
  , test4_numberLookup
  ]
  


test1_numberLookup =
  let fourPointFive = Val_Num $ R 4.5
  in  testCase "Lookup 4.5" $
        lookup fourPointFive testTypeIndex
          @?=
        HS.fromList [ anyType, anyNumber, isNum, gtNum1
                    , ltNum2, range1, range2]


test2_numberLookup =
  let oneThousand = Val_Num $ Z 1000
  in  testCase "Lookup 1000" $
        lookup oneThousand testTypeIndex
          @?=
        HS.fromList [ anyType, anyNumber, lt0OrGt10, gtNum1, gtNum2] 


test3_numberLookup =
  let zero = Val_Num $ Z 0
  in  testCase "Lookup 0" $
        lookup zero testTypeIndex
          @?=
        HS.fromList [anyType, anyNumber, ltNum2,
                    range1, range2, range3]


test4_numberLookup =
  let neg50 = Val_Num $ R (-50.0)
  in  testCase "Lookup -50.0" $
        lookup neg50 testTypeIndex
          @?=
        HS.fromList [anyType, anyNumber, ltNum1, ltNum2, lt0OrGt10]


