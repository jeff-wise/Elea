


module Test.Type where

import qualified Data.Set as Set


tests_Type = testGroup "Type Tests" []



tests_isType = testGroup "IsType" [tests_isSetTy]


tests_isSetTy = testGroup "Check Set Type" [
    testCase "" $ assert



test_setLengthTy = testGroup "Check Set Length" [
    testCase "Empty set has size type of 0" $ assert $
      isType (SetWithLen 0) (Set Set.empty)
  , testCase "Singleton set has size type of 1" $ assert $
      isType (SetWithLen 1) (Set Set.singleton $ Z 10)
  , testCase "Empty set not same size type as sing set" $ assert $
      isType (SetWithLen 1) (Set Set.empty)
  , testCase "Set with 2 elements has size type of 2" $ assert $
      isType (SetWithLen 2) (Set $ Set.fromList [Z 1, Z 2])
  , testCase "Set of 4 sets has size type of 4" $ assert $
      isType (SetWithLen 4) (Set $ Set.fromList 
        [ Set Set.empty, 
        , Set (Set.singleton $ 100)
        , Set (Set.fromList $ [Z 1, Z 2, Pair (Z 4) (Z 5)])
        , Set (Set.fromList $ [Text "cat", Text "dog"])
        ]
      )

--



-- small check 
