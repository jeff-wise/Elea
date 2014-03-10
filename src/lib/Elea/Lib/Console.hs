

-- | Standard Library for STDIN/OUT
module Elea.Lib.Console where



import Data.Text as T



-- Existential forces = implied systems
--   Draw as solid systems
-- Structural forces = hypothetical systems
--   Draw as faded/dotted line systems
--   change permeabilty to be top-level OR?
--   tag types?



createConsoleSystem ∷ Cons_System
createConsoleSystem locPath = Cons_System
  { _sysConsLoc = locPath
  , _sysConsMem = createConsoleMembrane
  , _sysConsIni = createConsoleActions
  }


createConsoleMembrane = Cons_Membrane
  { _memValue = dict [
                  ("id", Val_Text $ Text "consoleIO")
                ]
  , _memInter = Nothing
  , _memCnstr = Cons_Constraints
      { constUnique = []
      , constCells  = synTypeConst consoleCellTy
      }
  }



-- | A cell is either a message or a request.
consoleCellTy ∷ Type
consoleCellTy = Ty_Or $ OrTy [
    Ty_Dict $ AndTy [
      Ty_Dict $ HasEntry "type" (Ty_Text $ IsText "message")
    , Ty_Or $ OrTy [
        Ty_Dict $ HasEntry "string" (Ty_Text AnyText)
      , Ty_Dict $ HasEntry "lines"
          (Ty_Arr $ EachArrValue (Ty_Text AnyText))
      ]
    , Ty_Dict $ DictOfSize 2
    ]
  , Ty_And $ AndTy [
      Ty_Dict $ HasEntry "type" (Ty_Text $ IsText "request")
    , Ty_Dict $ HasEntry "prompt" (Ty_Text AnyText)
    , Ty_Dict $ HasEntry "return" (Ty_URI URITy)
    , Ty_Dict $ DictOfSize 3
    ]
  ]




-- ||
-- ||  Inside the console system
-- \/
-- Create the Writer and the Requestor particles

createConsoleActions ∷ [Effect]
createConsoleActions = [
    Effect {
      _effectPrec   = 0
    , _effectForces = [
        Force_Create $ Src_Cons createWriter
      , Force_Create $ Src_Cons createRequestor
      ]
    }  
  ]





createWriter ∷ Cons_System
createWriter = Cons_System
  { _sysConsLoc = "This"
  , _sysConsMem = Cons_Membrane
      { _memValue = dict [
                      ("id", Val_Text $ Text "writer")
                    ]
      , _memInter = Just $ Cons_Interaction
          { _consInterCause   = writeCause
          , _consInterEffect  = [
                Effect {
                  _effectPrec = 0
                , _effectForces = [
                      Force_IO $ IOPerform writeMessage
                    ]
                }
              ]
          }
      , _memCnstr = particle
      }
  , _sysConsIni = []
  }



createRequester ∷ Cons_System
createRequester = Cons_System
  { _sysConsLoc = "This"
  , _sysConsMem = Cons_Membrane
      { _memValue = dict [
                      ("id", Val_Text $ Text "requestor")
                    ]
      , _memInter = Just $ Cons_Interaction
          { _consInterCause   = requestCause
          , _consInterEffect  = [ Effect 0 sendRequest ]
          }
      , _memCnstr = particle
      }
  , _sysConsIni = []
  }




writeCause ∷ Cons_Cause
writeCause = Cons_OnNewCell $ synTypeConst $
  Ty_Dict $ HasEntry "type" (Ty_Text $ IsText "message")



requestCause ∷ Cons_Cause
requestCause = Cons_OnNewCell $ synTypeConst $
  Ty_Dict $ HasEntry "type" (Ty_Text $ IsText "request")




-- | Write the contents of a message to STDOUT
writeMessage ∷ Value → IO ()
writeMessage (Val_Dict $ Dict dictHM) = do
  maybe (return ()) writeString (HMS.lookup "string" dictHM)
  maybe (return ()) writeLines  (HMS.lookup "lines" dictHM)
  where
    writeString (Val_Text text) = T.putStr text
    writeLines (Val_Arr arr) = mapM_ T.putStrLn arr

   


sendRequest ∷ Value → IO Text
sendRequest 

Ty_Dict $ HasEntry "type" (Ty_Text $ IsText "request")
    , Ty_Dict $ HasEntry "prompt" (Ty_Text AnyText)
    , Ty_Dict $ HasEntry "return" (Ty_URI URITy)
    , Ty_Dict $ DictOfSize 3
   

