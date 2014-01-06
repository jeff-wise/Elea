


module Test.Lang.Type where


import Test.Prelude
import Test.Lang.Types

import Elea.Lang.Type
import Elea.Lang.Types


import qualified Data.HashSet as Set



tests_Type ∷ TestTree
tests_Type = testGroup "Type Tests" [tests_isType]



tests_isType ∷ TestTree
tests_isType = testGroup "IsType" [
    tests_isSetTy
  , tests_isNumTy
  ]



---------------------------------------------------------------------
-- Set Type Checking
---------------------------------------------------------------------

tests_isSetTy ∷ TestTree
tests_isSetTy = testGroup "Check Set Type" [
    tests_setLengthTy ]


tests_setLengthTy ∷ TestTree
tests_setLengthTy = testGroup "Check Set Length" [
    testCase "Empty set has size type of 0" $ assert $
      isType (Ty_Set $ SetWithSize $ Z 0) (Val_Set $ Set Set.empty)
  , testCase "Singleton set has size type of 1" $ assert $
      isType  (Ty_Set $ SetWithSize $ Z 1) $
              (Val_Set . Set . Set.singleton) (Val_Num $ Z 10)
  , testCase "Empty set not same size type as sing set" $ assert $
      not $ isType (Ty_Set $ SetWithSize $ Z 1) (Val_Set $ Set Set.empty)
  , testCase "Set with 2 elements has size type of 2" $ assert $
      isType  (Ty_Set $ SetWithSize $ Z 2) 
              (Val_Set $ Set $ Set.fromList [Val_Num $ Z 1, Val_Num $ Z 2])
  , testCase "Cardinality of complex set is 5" $ assert $
      isType (Ty_Set $ SetWithSize $ Z 5) complexSet

  ]


tests_isNumTy ∷ TestTree
tests_isNumTy = testGroup "Check Number Type" [
    tests_numEqTy
  , tests_numGtTy
  , tests_numLtTy
  , tests_numInRangeTy
  , tests_numEvenTy
  , tests_numOddTy
  , tests_numIntegerTy
  , tests_numNonNegativeTy
 -- , tests_numAnyTy
  ]


tests_numEqTy ∷ TestTree
tests_numEqTy = testGroup "Number Equality Type" [

    testCase "x = 0, where x = 0 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ Z 0) (Val_Num $ Z 0)

  , testCase "x = 0, where x = 1 is FALSE" $ assert $
      not $ isType (Ty_Num $ IsNumber $ Z 0) (Val_Num $ Z 1)

  , testCase "x = -10, where x = -10 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ Z (-10)) (Val_Num $ Z (-10))

  , testCase "x = 51, where x = 51 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ Z 51) (Val_Num $ Z 51)

  , testCase "x = 12.3, where x = 12.3 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ R 12.3) (Val_Num $ R 12.3)

  , testCase "x = 4.0, where x = 4 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ R 4.0) (Val_Num $ Z 4)

  , testCase "x = 4, where x = 4.0 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ Z 4) (Val_Num $ R 4.0)

  , testCase "x = -8, where x = -8.0 is TRUE" $ assert $
      isType (Ty_Num $ IsNumber $ Z (-8)) (Val_Num $ R (-8.0))

  , testCase "x = 1, where x = 1.0000000001 is FALSE" $ assert $
      not $ isType (Ty_Num $ IsNumber $ Z 1) (Val_Num $ R 1.0000000001)

    -- TODO: test float precision, integer overflow
  ]


tests_numGtTy ∷ TestTree
tests_numGtTy = testGroup "Number Greater-Than Type" [

    testCase "x > 0, where x = 1 is TRUE" $ assert $
      isType (Ty_Num $ GreaterThan $ Z 0) (Val_Num $ Z 1)

  , testCase "x > 0, where x = -1 is FALSE" $ assert $
      not $ isType (Ty_Num $ GreaterThan $ Z 0) (Val_Num $ Z (-1))

  , testCase "x > 0, where x = 0 is FALSE" $ assert $
      not $ isType (Ty_Num $ GreaterThan $ Z 0) (Val_Num $ Z 0)
      
  , testCase "x > 1.0, where x = 2 is TRUE" $ assert $
      isType (Ty_Num $ GreaterThan $ R 1.0) (Val_Num $ Z 2)
  
  , testCase "x > 1, where x = -2.0 is FALSE" $ assert $
      not $ isType (Ty_Num $ GreaterThan $ Z 1) (Val_Num $ R (-2.0))

  , testCase "x > 1, where x = 1.00000000001 is TRUE" $ assert $
      isType (Ty_Num $ GreaterThan $ Z 1) (Val_Num $ R 1.00000000001)
  ]



tests_numLtTy ∷ TestTree
tests_numLtTy = testGroup "Number Less-Than Type" [

    testCase "x < 0, where x = -1 is TRUE" $ assert $
      isType (Ty_Num $ LessThan $ Z 0) (Val_Num $ Z (-1))

  , testCase "x < 0, where x = 1 is FALSE" $ assert $
      not $ isType (Ty_Num $ LessThan $ Z 0) (Val_Num $ Z 1)

  , testCase "x < 0, where x = 0 is FALSE" $ assert $
      not $ isType (Ty_Num $ LessThan $ Z 0) (Val_Num $ Z 0)
      
  , testCase "x < 2.0, where x = 1 is TRUE" $ assert $
      isType (Ty_Num $ LessThan $ R 2.0) (Val_Num $ Z 1)
  
  , testCase "x < 1, where x = 2.0 is FALSE" $ assert $
      not $ isType (Ty_Num $ LessThan $ Z 1) (Val_Num $ R 2.0)

  , testCase "x < 1, where x = 0.00000000009 is TRUE" $ assert $
      isType (Ty_Num $ LessThan $ Z 1) (Val_Num $ R 0.00000000009)
  ]


tests_numInRangeTy ∷ TestTree
tests_numInRangeTy = testGroup "Number In Range Type" [

    testCase "x in [0,0], where x = 0 is TRUE" $ assert $
      isType (Ty_Num $ InRange (Z 0) (Z 0)) (Val_Num $ Z 0)

  , testCase "x in [0,1], where x = 1 is TRUE" $ assert $
      isType (Ty_Num $ InRange (Z 0) (Z 1)) (Val_Num $ Z 1)

  , testCase "x in [0,1], where x = 2 is FALSE" $ assert $
      not $ isType (Ty_Num $ InRange (Z 0) (Z 1)) (Val_Num $ Z 2)

  , testCase "x in [-5,-2], where x = 0 is FALSE" $ assert $
      not $ isType (Ty_Num $ InRange (Z (-5)) (Z (-2))) (Val_Num $ Z 0)

  , testCase "x in [-5,-2], where x = -3 is TRUE" $ assert $
      isType (Ty_Num $ InRange (Z (-5)) (Z (-2))) (Val_Num $ Z (-3))

  , testCase "x in [-1.0, 1.05], where x = 0 is TRUE" $ assert $
      isType (Ty_Num $ InRange (R (-1.0)) (R 1.05)) (Val_Num $ Z 0)

  , testCase "x in [-1.0, 2], where x = 0.075 is TRUE" $ assert $
      isType (Ty_Num $ InRange (R (-1.0)) (Z 2)) (Val_Num $ R (-0.075))
  ]



tests_numEvenTy ∷ TestTree
tests_numEvenTy = testGroup "Number Even Type" [

    testCase "x is even, where x = 0 is TRUE" $ assert $
     isType (Ty_Num Even) (Val_Num $ Z 0)

  , testCase "x is even, where x = 0.0 is TRUE" $ assert $
     isType (Ty_Num Even) (Val_Num $ R 0.0)

  , testCase "x is even, where x = 100.0 is TRUE" $ assert $
     isType (Ty_Num Even) (Val_Num $ R 100.0)

  , testCase "x is even, where x = 3 is FALSE" $ assert $
     not $ isType (Ty_Num Even) (Val_Num $ Z 3)

  , testCase "x is even, where x = 5.0 is FALSE" $ assert $
     not $ isType (Ty_Num Even) (Val_Num $ R 5.0)

  , testCase "x is even, where x = 2.5 is FALSE" $ assert $
     not $ isType (Ty_Num Even) (Val_Num $ R 2.5)
  ]



tests_numOddTy ∷ TestTree
tests_numOddTy = testGroup "Number Odd Type" [

    testCase "x is odd, where x = 1 is TRUE" $ assert $
     isType (Ty_Num Odd) (Val_Num $ Z 1)

  , testCase "x is odd, where x = 1.0 is TRUE" $ assert $
     isType (Ty_Num Odd) (Val_Num $ R 1.0)

  , testCase "x is odd, where x = 101.0 is TRUE" $ assert $
     isType (Ty_Num Odd) (Val_Num $ R 101.0)

  , testCase "x is odd, where x = 4 is FALSE" $ assert $
     not $ isType (Ty_Num Odd) (Val_Num $ Z 4)

  , testCase "x is odd, where x = 4.0 is FALSE" $ assert $
     not $ isType (Ty_Num Odd) (Val_Num $ R 4.0)

  , testCase "x is odd, where x = 3.5 is FALSE" $ assert $
     not $ isType (Ty_Num Odd) (Val_Num $ R 3.5)
  ]



tests_numIntegerTy ∷ TestTree
tests_numIntegerTy = testGroup "Number Integer Type" [

    testCase "x in Z, where x = 3 is TRUE" $ assert $
      isType (Ty_Num Integer) (Val_Num $ Z 3)
    
  , testCase "x in Z, where x = 3.3 is FALSE" $ assert $
      not $ isType (Ty_Num Integer) (Val_Num $ R 3.3)
  
  , testCase "x in Z, where x = 0 is TRUE" $ assert $
      isType (Ty_Num Integer) (Val_Num $ Z 0)

  , testCase "x in Z, where x = -3 is TRUE" $ assert $
      isType (Ty_Num Integer) (Val_Num $ Z (-3))
  
  , testCase "x in Z , where x = -303.45 is FALSE" $ assert $
      not $ isType (Ty_Num Integer) (Val_Num $ R (-303.45))
  ]


tests_numNonNegativeTy ∷ TestTree
tests_numNonNegativeTy = testGroup "Number NonNegative Type" [

    testCase "x >= 0, where x = 10 is TRUE" $ assert $
      isType (Ty_Num NonNegative) (Val_Num $ Z 10)
  
  , testCase "x >= 0, where x = 0 is TRUE" $ assert $
      isType (Ty_Num NonNegative) (Val_Num $ Z 0)

  , testCase "x >= 0, where x = -303.45 is FALSE" $ assert $
      not $ isType (Ty_Num NonNegative) (Val_Num $ R (-303.45))
  ]


