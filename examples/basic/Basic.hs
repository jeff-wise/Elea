

-- | Basic Example
--
-- Add three values to a system. Those values are then added
-- to a basic template and printed to STDOUT.
module Main where


import Prelude

import Elea.Lang.Sem.Processor
import Elea.Lang.Sem.System
import Elea.Lang.Sem.Types

import Elea.Lang.Term.Force
import Elea.Lang.Term.Lens
import Elea.Lang.Term.System
import Elea.Lang.Term.Transformer
import Elea.Lang.Term.Type
import Elea.Lang.Term.Value


import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.HashMap.Strict as HMS





----------------------- RECEPTORS --------------------------

-- A receptor which reacts to a text particle 
recpDefInputA ∷ ReceptorDefintion
recpDefInputA = ReceptorDef "signal_inputA" $
  Ty_Rec $ HasEntry "inputA" (Ty_Txt AnyText)



-- A receptor which reacts to a number particle 
recpDefInputB ∷ ReceptorDefintion
recpDefInputB = ReceptorDef "signal_inputB" $
  Ty_Rec $ HasEntry "inputB" (Ty_Num AnyNumber)


-- A receptor which reacts to a record particle 
recpDefInputC ∷ ReceptorDefintion
recpDefInputC = ReceptorDef "signal_inputC" $
  Ty_Rec $ HasEntry "inputC" (Ty_Rec AnyRecord)




--------------------- ACTION POTENTIALS --------------------

apDef ∷ APDefinition
apDef = APDef
          -- Identifies the action potential
          "printParams"
           -- AP fires after receiving these three signals
          ["signal_inputA", "signal_inputB", "signal_inputC"]
          -- Signals/events are multiplexed over unique user names
          (Lens_Rec $ AtLabel "user" Lens_This)
          -- The only effect of this action potential is a force
          -- which takes the parameters and prints them to stdout
          ["PrintParams"]
  



--------------------------- FORCES -------------------------

printParams ∷ Force
printParams = F_IO $ IOPerform printParams'

  where

    printParams' ∷ ParamMap → IO ()
    printParams' paramMap =
      let consParamTexts accText signal paramValue =
            accText ++ show signal ++ "\n"
                    ++ show paramValue ++ "\n"
      in  putStrLn $ "Parameters: " ++
              HMS.foldlWithKey' consParamTexts "" paramMap



-- | Project all values into the Basic (only) system
basicProjection = Projection Lens_This "basic"


synInputA ∷ Force
synInputA = F_Syn $ Syn inputAValue [basicProjection]
  where
    inputAValue = Tr_Template $ T_Rec $ RecT $ HMS.fromList
      [ ("user"  , T_Var "user"             )
      , ("inputA", T_Txt $ TxtT $ Txt "Lion")
      ]



synInputB ∷ Force
synInputB = F_Syn $ Syn inputBValue [basicProjection]
  where
    inputBValue = Tr_Template $ T_Rec $ RecT $ HMS.fromList
      [ ("user"   , T_Var "user")
      , ("inputB" , T_Num $ NumT $ R 673.33)
      ]
    


synInputC ∷ Force
synInputC = F_Syn $ Syn inputCValue [basicProjection]
  where
    inputCValue = Tr_Template $ T_Rec $ RecT $ HMS.fromList
      [ ("user"   , T_Var "user" )
      , ("inputC" , T_Rec $ RecT $ HMS.fromList
                      [ ("gender", T_Txt $ TxtT $ Txt "Male")
                      , ("age"   , T_Var "age"              )
                      ]
        )
      ]
                      


forceMap ∷ ForceMap
forceMap = HMS.fromList
  [ ("PrintParams", printParams )
  , ("synInputA"  , synInputA   )
  , ("synInputB"  , synInputB   )
  , ("synInputC"  , synInputC   )
  ]




------------------------- SYSTEM -------------------------


createBasicSystem ∷ STM System
createBasicSystem = do
  system ← newSystem
  addReceptor system recpDefInputA
  addReceptor system recpDefInputB
  addReceptor system recpDefInputC
  addActionPotential system apDef
  return system

  



--------------------------- MAIN ---------------------------

-- | Build the universe and run the three initial forces.
-- Those forces should trigger the IO force to print out its
-- parameters, which are the three values created by the initial
-- synthesis forces.
main ∷ IO ()
main = do
  effectQueue ← atomically $ newEffectQueue
  system ← atomically $ createBasicSystem
  let systemMap = HMS.singleton "basic" system
      universe = Univ systemMap forceMap effectQueue
      sendEffect = flip queueEffect
  -- Start the processor in its own thread to wait for effects
  _ ← forkIO $ processor universe
  -- Run the test forces by feeding effects to the processor
  atomically $ do
    -- Chase creates input A
    sendEffect universe $
      Effect
        (HMS.singleton "user" (Val_Txt $ Txt "Chase"))
        ["synInputA"]
    -- Wilson creates input B
    sendEffect universe $
      Effect
        (HMS.singleton "user" (Val_Txt $ Txt "Wilson"))
        ["synInputB"]
    -- Chase creates input C
    sendEffect universe $
      Effect
        (HMS.fromList [ ("user" , Val_Txt $ Txt "Chase")
                      , ("age"  , Val_Num $ Z 25       )
                      ]) 
        ["synInputC"]
    -- Wilson creates input A
    sendEffect universe $
      Effect
        (HMS.singleton "user" (Val_Txt $ Txt "Wilson"))
        ["synInputA"]
    -- Chase creates input B
    sendEffect universe $
      Effect
        (HMS.singleton "user" (Val_Txt $ Txt "Chase"))
        ["synInputB"]
    -- Wilson creates input C
    sendEffect universe $
      Effect
        (HMS.fromList [ ("user" , Val_Txt $ Txt "Wilson")
                      , ("age"  , Val_Num $ Z 41        )
                      ]) 
        ["synInputC"]
  -- Pause to allow effects to process and return when
  -- the user presses any key
  getLine >> return ()

