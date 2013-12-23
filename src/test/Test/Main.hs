

module Main where


import Test.Type (tests_Type)


do = defaultMain tests



tests = testGroup "Tests" [unitTests]


unitTests = testGroup "Unit Tests" [tests_Type]
