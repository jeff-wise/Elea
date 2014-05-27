


module EleaTest.Lang.Sem.ValueIndex (
    tests_lens
  ) where



import EleaTest.Prelude


import Elea.Lang.Term.Lens
import Elea.Lang.Sem.Lens





-- | Value Index tests
tests_lens =  testGroup "Value Index" [ tests_get, tests_put]
 


tests_get = testGroup "Lens GET" [ get_this, get_entry, get_index ]

tests_put = testGroup "Lens PUT" [ put_this, put_entry, put_index ]




------------------------ TEST VALUES -----------------------

one ∷ Value
one = Val_Num $ Z 1

two ∷ Value
two = Val_Num $ Z 2

three ∷ Value
three = Val_Num $ Z 3

five ∷ Value
five = Val_Num $ Z 5

ten ∷ Value
ten = Val_Num $ Z 10


record ∷ Value
record = Val_Rec $ Rec $ HMS.fromList
            [ ("a", one  )
            , ("b", two  )
            , ("c", three)
            ]


array ∷ Value
array = Val_Arr $ Arr $ Seq.fromList [one, two, three]




---------------------------- GET ---------------------------

get_this =
  let thisLens = Lens_This
  in  testCase "Get THIS" $
        lens Get thisLens five
          @?= 
        Just five


get_entry =
  let entryLens = Lens_Rec $ AtLabel "a" Lens_This
  in  testCase "Get 'a' " $
        lens Get entryLens record
          @?= 
        Just one



get_index =
  let indexLens =  Lens_Arr $ AtIndex 1 Lens_This
  in  testCase "Get 1" $
        lens Get indexLens array
          @?=
        Just two



---------------------------- PUT ---------------------------

put_this =
  let thisLens = Lens_This
  in  testCase "Put 10" $
        lens (Put ten) thisLens five
          @?=
        Just ten



put_entry =
  let entryLens = Lens_Rec $ AtLabel "b" Lens_This
  in  testCase "Put 5 @ 'b' " $
        lens (Put five) entryLens record
          @?= 
        (Just $ Val_Rec $ Rec $ HMS.fromList
            [ ("a", one  )
            , ("b", five )
            , ("c", three)
            ]
        )


put_index = 
  let indexLens =  Lens_Arr $ AtIndex 2 Lens_This
  in  testCase "Put 10 @ 2" $
        lens (Put ten) thisLens five
          @?=
        ( Just $ Val_Arr $ Arr $ Seq.fromList [ one, two, ten ] )



