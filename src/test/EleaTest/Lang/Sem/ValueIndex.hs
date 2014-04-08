

-- |
-- Value Index Tests
--
-- Create a value index with some sensible default values.
--
-- Ignore the internal representation. 
-- 
-- Test that it works by performing lookups for various
-- types. If all the lookups are successful, by returning
-- all values which match a given type, then the index
-- is semantically correct.
--
-- Account for edge cases as much as possible, but right now
-- the type/value specification is still subject to change,
-- so most tests will only cover basic functionality.
module EleaTest.Lang.Sem.ValueIndex (
    tests_ValueIndex
  ) where



import EleaTest.Prelude

import Elea.Lang.Term.Value
import Elea.Lang.Term.Type
import Elea.Lang.Sem.ValueIndex


import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Sequence as Seq



-- | Value Index tests
tests_ValueIndex =  testGroup "Value Index"
  [ tests_lookupRecordType
  , tests_lookupArrayType
  , tests_lookupTextType
  , tests_lookupNumberType
  , tests_lookupAndType
  , tests_lookupOrType
  ]




----------------------- TEST VALUES ------------------------

-- ***** Records

simpleRecord = Val_Rec $ Rec $ HMS.fromList
  [ ("name", Val_Txt $ Text "James") 
  , ("age", Val_Num $ Z 8) 
  ]

fancyRecord = Val_Rec $ Rec $ HMS.fromList 
  [ ("type", Val_Txt $ Text "post")
  , ("name", Val_Txt $ Text "James")
  , ("tags" , Val_Arr $ Arr $ Seq.fromList
      [ Val_Txt $ Text "hiking"
      , Val_Txt $ Text "alps"
      , Val_Txt $ Text "honeymoon"
      ]
    )
  , ("comments", Val_Rec $ Rec $ HMS.fromList
      [ ("Sara", Val_Txt $ Text "Nice Post!")
      , ("Ron" , Val_Txt $ Text "So cool." )
      ]
    )
  ]



-- ***** Arrays

simpleArray = Val_Arr $ Arr $ Seq.fromList
  [ zero
  , Val_Txt $ Text "two"
  , Val_Num $ R 3.0
  ]


fancyArray = Val_Arr $ Arr $ Seq.fromList
  [ zero
  , dogText
  , simpleRecord
  , simpleArray
  ]



-- ***** Texts

telescopeText = Val_Txt $ Text "telescope"
christmasText = Val_Txt $ Text "Christmas"
dogText = Val_Txt $ Text "dog"



-- ***** Numbers

zero        = Val_Num $ Z 0
zero2       = Val_Num $ R 0.0
one         = Val_Num $ Z 1
negOne      = Val_Num $ Z (-1)
piNum       = Val_Num $ R 3.14
bigRealNeg  = Val_Num $ R (-456.789)
bigRealPos  = Val_Num $ R 79.8888889
bigIntPos   = Val_Num $ Z 9888898
bigIntNeg   = Val_Num $ Z (-312818)



--------------------- TEST VALUE INDEX ---------------------

testValueIndex ∷ ValueIndex
testValueIndex =
  let insertValues =  insert simpleRecord
                  >>> insert fancyRecord
                  >>> insert simpleArray
                  >>> insert fancyArray
                  >>> insert telescopeText
                  >>> insert christmasText
                  >>> insert dogText
                  >>> insert zero
                  >>> insert zero2
                  >>> insert one
                  >>> insert negOne
                  >>> insert piNum
                  >>> insert bigRealNeg
                  >>> insert bigRealPos
                  >>> insert bigIntPos
                  >>> insert bigIntNeg
  in  insertValues newValueIndex




--------------------- RECORD TYPE LOOKUP -------------------

tests_lookupRecordType = testGroup "Record Type Lookup"
  [ test1_recordTypeLookup 
  , test2_recordTypeLookup
  , test3_recordTypeLookup
  , test4_recordTypeLookup
  , test5_recordTypeLookup
  ]
                               


test1_recordTypeLookup = 
  let isPostType =  Ty_Rec $ HasEntry "type"
                      (Ty_Txt $ IsText $ Text "post")
  in  testCase "Is a Post" $
        lookup isPostType testValueIndex
          @?=
        HS.fromList [fancyRecord]


test2_recordTypeLookup = 
  let isAboutHiking = Ty_Rec $ HasEntry "tags"
                        (Ty_Arr $ WithIndex 0
                          (Ty_Txt $ IsText $ Text "hiking"))
  in  testCase "Post about Hiking" $
        lookup isAboutHiking testValueIndex
          @?=
        HS.fromList [fancyRecord]


test3_recordTypeLookup =
  let byJames = Ty_Rec $ HasEntry "name"
                  (Ty_Txt $ IsText $ Text "James")
  in  testCase "Name of James" $
        lookup byJames testValueIndex
          @?=
        HS.fromList [simpleRecord, fancyRecord]
  

test4_recordTypeLookup =
  let recSizeTwo = Ty_Rec $ WithSize 2
  in  testCase "Record with size 2" $
        lookup recSizeTwo testValueIndex
          @?=
        HS.fromList [simpleRecord]
  

test5_recordTypeLookup =
  let anyRecord = Ty_Rec AnyRecord
  in  testCase "Name of James" $
        lookup anyRecord testValueIndex
          @?=
        HS.fromList [simpleRecord, fancyRecord]




--------------------- ARRAY TYPE LOOKUP --------------------

tests_lookupArrayType = testGroup "Array Type Lookup"
  [ test1_arrayTypeLookup 
  , test2_arrayTypeLookup
  , test3_arrayTypeLookup
  , test4_arrayTypeLookup
  , test5_arrayTypeLookup
  ]



test1_arrayTypeLookup =
  let hasDog = Ty_Arr $ WithIndex 1
                  (Ty_Txt $ IsText $ Text "dog")
  in  testCase "Has Dog" $
        lookup hasDog testValueIndex
          @?=
        HS.fromList [fancyArray]
  

test2_arrayTypeLookup =
  let hasZero = Ty_Arr $ WithIndex 0 (Ty_Num $ IsNumber $ Z 0)
  in  testCase "Has Zero" $
        lookup hasZero testValueIndex
          @?=
        HS.fromList [simpleArray, fancyArray]


test3_arrayTypeLookup =
  let withAgeEight =  Ty_Arr $ WithIndex 2
                        (Ty_Rec $
                          HasEntry "age"
                            (Ty_Num $ IsNumber $ Z 8))
  in  testCase "Has Record of Age 8" $
        lookup withAgeEight testValueIndex
          @?=
        HS.fromList [fancyArray]


test4_arrayTypeLookup =
  let withLengthThree =  Ty_Arr $ WithArrLen 3
  in  testCase "Length of 3" $
        lookup withLengthThree testValueIndex
          @?=
        HS.fromList [simpleArray]


test5_arrayTypeLookup =
  let anyArray =  Ty_Arr AnyArray
  in  testCase "Is Array" $
        lookup anyArray testValueIndex
          @?=
        HS.fromList [simpleArray, fancyArray]




--------------------- TEXT TYPE LOOKUP ---------------------

tests_lookupTextType = testGroup "Text Type Lookup"
  [ test1_textTypeLookup 
  , test2_textTypeLookup
  , test3_textTypeLookup
  ]



test1_textTypeLookup =
  let isTelescope = Ty_Txt $ IsText $ Text "telescope"
  in  testCase "Is Telescope" $
        lookup isTelescope testValueIndex
          @?=
        HS.fromList [telescopeText]
  

test2_textTypeLookup =
  let textWithLengthNine = Ty_Txt $ WithTextLen 9
  in  testCase "Text with length 9" $
        lookup textWithLengthNine testValueIndex
          @?=
        HS.fromList [telescopeText, christmasText]


test3_textTypeLookup =
  let anyText = Ty_Txt AnyText
  in  testCase "Any Text" $
        lookup anyText testValueIndex
          @?=
        HS.fromList [telescopeText, christmasText, dogText]




--------------------- TEXT TYPE LOOKUP ---------------------

tests_lookupNumberType = testGroup "Number Type Lookup"
  [ test1_numberTypeLookup
  , test2_numberTypeLookup
  , test3_numberTypeLookup
  , test4_numberTypeLookup
  , test5_numberTypeLookup
  , test6_numberTypeLookup
  ]


test1_numberTypeLookup =
  let isZero = Ty_Num $ IsNumber $ Z 0
  in  testCase "Is zero" $
        lookup isZero testValueIndex
          @?=
        HS.fromList [zero, zero2]
 

test2_numberTypeLookup =
  let isBigRealNeg = Ty_Num $ IsNumber $ R (-456.789)
  in  testCase "Is -456.789" $
        lookup isBigRealNeg testValueIndex
          @?=
        HS.fromList [bigRealNeg]
 

test3_numberTypeLookup =
  let greaterThanOne = Ty_Num $ GreaterThan $ Z 1
  in  testCase "Greater than one" $
        lookup greaterThanOne testValueIndex
          @?=
        HS.fromList [piNum, bigRealPos, bigIntPos]


test4_numberTypeLookup =
  let lessThanOneHundred = Ty_Num $ LessThan $ R 100.0
  in  testCase "Less than 100" $
        lookup lessThanOneHundred testValueIndex
          @?=
        HS.fromList [zero, zero2, one, negOne,
                  piNum, bigRealNeg, bigRealPos, bigIntNeg]


test5_numberTypeLookup =
  let btwZeroAndFive = Ty_Num $ Range (Z 0) (Z 5)
  in  testCase "In range [0, 5]" $
        lookup btwZeroAndFive testValueIndex
          @?=
        HS.fromList [zero, zero2, one, piNum]


test6_numberTypeLookup =
  let anyNumber = Ty_Num AnyNumber
  in  testCase "Any Number" $
        lookup anyNumber testValueIndex
          @?=
        HS.fromList [zero, zero2, one, negOne, piNum,
                    bigRealNeg, bigRealPos, bigIntPos, bigIntNeg]




---------------------- AND TYPE LOOKUP ---------------------

tests_lookupAndType = testGroup "And Type Lookup"
  [ test1_andTypeLookup
  , test2_andTypeLookup
  , test3_andTypeLookup
  , test4_andTypeLookup
  ]



test1_andTypeLookup =
  let gtZeroAndEqPi = Ty_And $ AndType
                        [ Ty_Num $ IsNumber $ R 3.14
                        , Ty_Num $ GreaterThan $ Z 0 ]
  in  testCase "Greater than 0 and equal to PI" $
        lookup gtZeroAndEqPi testValueIndex
          @?=
        HS.fromList [piNum]


test2_andTypeLookup =
  let eqZeroAndOne =  Ty_And $ AndType
                        [ Ty_Num $ IsNumber $ Z 0
                        , Ty_Num $ IsNumber $ Z 1 ]
  in  testCase "Equal to 0 and 1" $
        lookup eqZeroAndOne testValueIndex
          @?=
        HS.fromList []


test3_andTypeLookup =
  let postByJames = Ty_And $ AndType
                      [ Ty_Rec $ HasEntry "type"
                          (Ty_Txt $ IsText $ Text "post")
                      , Ty_Rec $ HasEntry "name"
                          (Ty_Txt $ IsText $ Text "James")
                      ]
  in  testCase "Post by James" $
        lookup postByJames testValueIndex
          @?=
        HS.fromList [fancyRecord]


test4_andTypeLookup =
  let arrLen3With3 =  Ty_And $ AndType
                        [ Ty_Arr $ WithArrLen 3
                        , Ty_Arr $ WithIndex 2 (Ty_Num $ IsNumber $ Z 3)
                        ]
  in  testCase "Array with length 3 and index 2 is 3" $
        lookup arrLen3With3 testValueIndex
          @?=
        HS.fromList [simpleArray]




----------------------- OR TYPE LOOKUP ---------------------

tests_lookupOrType = testGroup "Array Type Lookup"
  [ test1_orTypeLookup 
  , test2_orTypeLookup
  , test3_orTypeLookup
  , test4_orTypeLookup
  ]



test1_orTypeLookup =
  let isZeroOrPi =  Ty_Or $ OrType
                      [ Ty_Num $ IsNumber $ Z 0
                      , Ty_Num $ IsNumber $ R 3.14]
  in  testCase "Number is 0 or PI" $
        lookup isZeroOrPi testValueIndex
          @?=
        HS.fromList [zero, zero2, piNum]


test2_orTypeLookup =
  let isOneOrTen =  Ty_Or $ OrType
                      [ Ty_Num $ IsNumber $ Z 1
                      , Ty_Num $ IsNumber $ Z 10 ]
  in  testCase "Number is 1 or 10" $
        lookup isOneOrTen testValueIndex
          @?=
        HS.fromList [one]


test3_orTypeLookup =
  let has3orIsDog = Ty_Or $ OrType
                      [ Ty_Arr $ WithIndex 2
                          (Ty_Num $ IsNumber $ R 3.0)
                      , Ty_Txt $ IsText $ Text "dog"
                      ]
  in  testCase "Array with 3.0 or dog text" $
        lookup has3orIsDog testValueIndex
          @?=
        HS.fromList [simpleArray, dogText]


test4_orTypeLookup =
  let isArrayOrRecord =
        Ty_Or $ OrType [Ty_Arr AnyArray, Ty_Rec AnyRecord]
  in  testCase "Is array or record" $
        lookup isArrayOrRecord testValueIndex
          @?=
        HS.fromList [ simpleRecord, fancyRecord
                    , simpleArray, fancyArray]


