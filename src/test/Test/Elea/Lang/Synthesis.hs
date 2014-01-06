

module Test.Lang.Synthesis where


import Test.Prelude
import Test.Lang.Types

import Elea.Lang.Types
import Elea.Lang.Synthesis

import qualified Data.Sequence as Seq





tests_Synthesis = testGroup "Synthesis" [ test_syn_2plus2 ]
 


-- | Synthesize 2 + 2
test_syn_2plus2 = testCase "2 + 2" $ 

    -- Synthesize two "2" particles via addition
    (synthesize testSystem $ 
      Syn {
        _synName  = "2+2" 
      , _synCons  = [ Con_Val $ Val_Num $ Z 2
                    , Con_Val $ Val_Num $ Z 2 ]
      , _synApps  = Seq.fromList [
                      App {
                        _appName    = Nothing 
                      , _appParams  = [ Param 0 Lens_This   -- 2
                                      , Param 1 Lens_This ] -- 2
                      , _appFunName = "Add"
                      , _resultId   = sym_sum
                      }
                    ]
      , _synPartT = Particle sym_sum (Val_Var $ Var sym_sum)
      })

  @?=

    -- Should create a "4" particle 
    Particle sym_sum (Val_Num $ Z 4)
 


-- | Basic Report
-- query?






