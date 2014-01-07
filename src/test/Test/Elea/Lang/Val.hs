

module Test.Elea.Lang.Val where


import Test.Prelude
import Test.Data.System

import Elea.Lang.Val
import Elea.Lang.Types


import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as Set






tests_Val = testGroup "Val Tests" [
  tests_Eval ]



tests_Eval = testGroup "Eval" [
                              
    testCase "Simple Eval" $
          eval  (HMS.singleton (Val_Sym $ sym_social^.username) 
                              (Val_Text $ Text "BroRex123")
                )
                (Val_Var $ Var $ Val_Sym $ sym_social^.username)
      @?= (Val_Text $ Text "BroRex123")

  , testCase "Eval in Set" $
          eval  (HMS.singleton  (Val_Sym $ sym_social^.username)
                                (Val_Text $ Text "BroRex123")
                )
                (Val_Set $ Set $ Set.fromList [
                    Val_Num $ Z 5
                  , Val_Var $ Var $ Val_Sym $ sym_social^.username
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
                    (Val_Sym $ sym_rpg^.attr.health, Val_Num $ Z 49)
                  , (Val_Sym $ sym_rpg^.attr.gold, Val_Num $ Z 14)
                  ]
                )
                (Val_Set $ Set $ Set.fromList [
                    Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.name) 
                                    (Val_Text $ Text "Sky")
                  , Val_Pair $ Pair 
                        (Val_Sym $ sym_rpg^.entity.dragon)
                        (Val_Set $ Set $ Set.fromList [
                            Val_Pair $ Pair
                              (Val_Sym $ sym_rpg^.attr.health) 
                              (Val_Var $ Var $ Val_Sym $ sym_rpg^.attr.health)
                          , Val_Pair $ Pair
                              (Val_Sym $ sym_rpg^.attr.gold)
                              (Val_Var $ Var $ Val_Sym $ sym_rpg^.attr.gold)
                          ]
                        )
                  ]
                )
      @?= (Val_Set $ Set $ Set.fromList [
              Val_Pair $ Pair (Val_Sym $ sym_rpg^.attr.name)
                              (Val_Text $ Text "Sky")
            , Val_Pair $ Pair 
                (Val_Sym $ sym_rpg^.entity.dragon)
                (Val_Set $ Set $ Set.fromList [
                        Val_Pair $ Pair
                          (Val_Sym $ sym_rpg^.attr.health) 
                          (Val_Num $ Z 49)
                      , Val_Pair $ Pair
                          (Val_Sym $ sym_rpg^.attr.gold)
                          (Val_Num $ Z 14)
                  ]
                )
            ]
          )
  ]

