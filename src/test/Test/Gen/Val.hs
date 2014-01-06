

module Test.Gen.Val where


import Test.Prelude

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen



data Val = Val_Num Number
        | Val_Pair Pair

data Number = Z Int | R Double

data Pair = Pair Val Val


data Type = Ty_Num NumberTy
          | Ty_Pair PairTy



data PairTy = 
    IsPair  Type Type
  | First   Type
  | Second  Type
  | AnyPair


data NumberTy = 
    IsNumber      Number
  | GreaterThan   Number
  | LessThan      Number
  | InRange       Number Number
  | Even
  | Odd
  | Integer
  | NonNegative
  | AnyNumber


data ValTypes = ValTypes
  { _isTypes    ∷  [Type]
  , _isNotTypes ∷  [Type]
  }


data TestNumber = TestNumber Number NumberTy

data TestPair = TestPair Pair PairTy


data TestVal = TestVal Val [Type]




instance Arbitrary TestVal where
  arbitrary = 
    let arbNum  = do
          (TestNumber num numTys) ← arbitrary ∷ Gen TestNumber
          return $ TestVal (Val_Num num) (map Ty_Num numTys)
        arbPair  = do 
          (TestPair pair pairTys) ← arbitrary ∷ Gen TestPair
          return $ TestVal (Val_Pair pair) (map Ty_Pair pairTys)
    in  oneof [arbNum, arbPair]




instance Arbitrary TestNumber where
  arbitrary = oneof [
                  int_zero
                , int_one
              ]



int_zero ∷ Gen TestNumber
int_zero =
  let zeroTypes = [ IsNumber (Z 0)
                  , IsNumber (R 0.0)
                  , InRange (Z (-2)) (R 87.5)
                  , InRange (Z 0) (R 0.0)
                  , InRange (R (-1.0)) (Z 0)
                  , InRange (R (-0.0000001)) (Z 0)
                  , GreaterThan (Z (-1))
                  , LessThan (R 2.0)
                  , Even
                  , Integer
                  , NonNegative
                  , AnyNumber
                  ]
 in  TestNumber <$> (return $ Z 0) <*> (elements zeroTypes)




int_one ∷ Gen TestNumber
int_one =
  let oneTypes  = [ IsNumber (Z 1)
                  , IsNumber (R 1.0)
                  , LessThan (Z 2)
                  , GreaterThan (R 0.999999999)
                  , InRange (Z 1) (R 3000.07)
                  , InRange (R -1.0) (R 1.0)
                  , Odd
                  , NonNegative
                  , AnyNumber
                  , Integer
                  ]
  in  TestNumber <$> (return $ Z 1) <*> (elements oneTypes)
