

module Test.Elea.Index.Val (tests_ValIndex) where



import Test.Prelude
import Test.Data.RPG
import Test.Data.System


import Elea.Index.Val
import Elea.Lang.Types



-- put things in
-- try types
-- see if correct things come out

-- test what structure looks like ?
-- does it really matter ?



tests_ValIndex =  testGroup "Val Index" [
                     tests_rpgValIndex 
                  --  , basicValIndex
                  ]
 

-- flip lookup to make writing tests neater
lookupF = flip lookup




rpgValIndex = 
  let addRpgVals =  insert (_partId dungeonP  ) dungeonP
                >>> insert (_partId cityP     ) cityP
                >>> insert (_partId bartenderP) bartenderP
                >>> insert (_partId heroP     ) heroP
                >>> insert (_partId warlockP  ) warlockP
                >>> insert (_partId dragonP   ) dragonP
  in  addRpgVals newValIndex
                               

tests_rpgValIndex = testGroup "RPG Val Index" [ 

    testCase "Dungeon exists in index" $
        lookupF rpgValIndex  (Ty_Sym $ IsSymbol $ sym_rpg^.entity.dungeon)
      @?=
        [dungeonP]

 {- , testCase "City exists in index" $
        lookupF rpgValIndex  (Ty_Sym $ IsSymbol $ sym_rpg^.entity.dungeon)
      @?=
        [dungeonP]
-}
  ]

 
