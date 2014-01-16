

module Test.Elea.Lang.Synthesis where


import Test.Prelude
import qualified Test.Data.System as Sys

import Elea.Lang.Types
import Elea.Lang.Synth.Synthesis


import Control.Monad.State.Lazy (runState)

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
                      , _resultId   = Val_Sym $ sym_sum
                      }
                    ]
      , _synPartT = Particle (Val_Text $ Text "Sum")
                             (Val_Var $ Var $ Val_Sym sym_sum)
      })

  @?=

    -- Should create a "4" particle 
    Particle (Val_Text $ Text "Sum") (Val_Num $ Z 4)
 


-- | Basic Report
-- query?






