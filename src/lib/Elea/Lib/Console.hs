

-- | Standard Library for STDOUT/IN
module Elea.Lib.Console where



-- Library is a language construct..
-- the only one which manipulates systems?
-- A way to store systems and automatically hosts them?
-- Repository, dep. management, etc..

consoleSystem parent =
  System
    <$> return consoleMembrane
    <*> emptyInterior
    <*> return parent



consoleMembrane = Membrane
  { _memValue = Val_Null
  , _memInter ∷ Interaction
  , _memCnstr = constraints
  }



constraints = Constraints
  { constUnique = []
  , constCells  = Ty_Or $ OrTy [

                  
  }


-- | A cell is either a message or a request.
consoleCellTy ∷ Type
consoleCellTy = Ty_Or $ OrTy [
    Ty_Dict $ IsDict [
      ("type"   , Ty_Text $ IsText "message")
    , ("string", Ty_Text AnyText)
    , ("return" , Ty_URI URITy)
  , Ty_Dict $ IsDict [
      ("type"   , Ty_Text $ IsText "request")
    , ("prompt" , Ty_Text AnyText)
    , ("return" , Ty_URI URITy)
    ]

  ]

-- request
--    prompt
--    response type
--
-- message
--  content
--  contentType: string/lines/

