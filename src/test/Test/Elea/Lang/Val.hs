

module Test.Lang.Val where


import Test.Prelude
import Test.Lang.Types

import Elea.Lang.Val
import Elea.Lang.Types


import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as Set






tests_Val = testGroup "Val Tests" [
  tests_Eval ]



tests_Eval = testGroup "Eval" [
                              
    testCase "Simple Eval" $
          eval (HMS.singleton sym_username (Val_Text $ Text "BroRex123"))
               (Val_Var $ Var sym_username)
      @?= (Val_Text $ Text "BroRex123")

  , testCase "Eval in Set" $
          eval  (HMS.singleton sym_username (Val_Text $ Text "BroRex123"))
                (Val_Set $ Set $ Set.fromList [
                    Val_Num $ Z 5
                  , Val_Var $ Var sym_username
                  , Val_Text $ Text "Dinosaurs"
                  ]
                )
      @?= (Val_Set $ Set $ Set.fromList [
              Val_Num $ Z 5
            , Val_Text $ Text "BroRex123"
            , Val_Text $ Text "Dinosaurs"
            ]
          )
      
  , testCase "Eval large value" $
          eval  (HMS.fromList [
                    (sym_health, Val_Num $ Z 49)
                  , (sym_arcana, Val_Num $ Z 14)
                  ]
                )
                (Val_Set $ Set $ Set.fromList [
                    Val_Pair $ Pair (sym_gold) (Val_Num $ Z 100)
                  , Val_Pair $ Pair 
                        (sym_stats) 
                        (Val_Set $ Set $ Set.fromList [
                            Val_Pair $ Pair (sym_health) 
                                            (Val_Var $ Var sym_health)
                          , Val_Pair $ Pair (sym_arcana)
                                            (Val_Var $ Var sym_arcana)
                          ]
                        )
                  ]
                )
      @?= (Val_Set $ Set $ Set.fromList [
              Val_Pair $ Pair  (sym_gold) (Val_Num $ Z 100)
            , Val_Pair $ Pair 
                    (sym_stats) 
                    (Val_Set $ Set $ Set.fromList [
                        Val_Pair $ Pair (sym_health) 
                                        (Val_Num $ Z 49)
                      , Val_Pair $ Pair (sym_arcana)
                                        (Val_Num $ Z 14)
                      ]
                    )
            ]
          )
  ]

