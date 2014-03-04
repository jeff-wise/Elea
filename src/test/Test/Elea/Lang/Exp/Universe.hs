


module Test.Elea.Lang.Exp.Universe where






test_Universe = testGroup "Universe" [
    test_CompProgram
  ]


-- just make sure all symbols are there
test_CompProgram = testGroup "Compile Actions" $
