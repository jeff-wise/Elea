

module Test.Elea.Lang.Exp.Query where


import Test.Prelude
import qualified Test.Data.Blog as B



-- initBlogContext 
-- need other contexts to test trigger query
-- init context trigger is main blog sys
-- blogUniv


test_Query = testGroup "Query" [
                test_ConstQuery
              , test_CtxQuery
              , test_TrigQuery
              , test_TypeQuery
              ]

 


test_ConstQuery = testGroup "Constant Query" [
    testCase "10" $
        (query initBlogContext (Query_Const $ Val_Num $ Z 10))
      @?=
        return $ (Val_Num $ Z 10)
  ]



test_CtxQuery = testGroup "Context Query" [

    testCase "Inital Blog Context" $
        (query Blog.initCtx Query_Ctx)
      @?=
        (Blog.univ^.symbol "Blog")
  ]



test_TrigQuery = testGroup "Trigger Value Query" [

    testCase "Initial Blog Context" $
        (query Blog.initCtx Query_Trig)
      @?=
        (Blog.univ^.symbol "Blog")
  ]


-- for this just need some child systems
test_TypeQuery = testGroup "Type Query" [

    testCase "Posts about food" $
        (query Blog.initCtx $
            Query_Ty $
              Ty_Set $ WithElem $ 
                pairTy (Ty_Sym $ IsSymbol (Blog.univ^.symbol $ "Tags"))
                       (Ty_Set $ WithElem $
                          Ty_Sym $ IsSymbol (Blog.univ^.symbol $ "Food"))
        )
      @?=
        (Val_Set $ Set $ HS.fromList [
                  Blog.foodPost1
                , Blog.foodPost2
                ]
        )
  ]


