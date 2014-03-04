

module Elea.Lang.Exp.Program where



import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Exp.Types
import qualified Elea.Lang.Atom.Index.Type as TI


import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust)
import qualified Data.List.Stream as L




programFromList ∷ [(Type, [(Val, [Action])])] → Program
programFromList rules = 
  L.foldl' (flip addRule) newProgram rules



-- name actions, unique id?

addRule ∷ (Type, [(Val, [Action])]) → Program → Program
addRule (ruleTy, actionList) =
    (over progRuleMap $
      HMS.insertWith (HMS.union) ruleTy (HMS.fromList actionList))
  >>>
    (over progRuleIdx $ TI.insert ruleTy)




-- | Given a program and a triggering value, find
-- any actions that would occur. If none, than that
-- triggering value is not permitted
-- TODO priorities etc..
reactions ∷ Program → Val → Maybe [Action]
reactions (Program progMap progIndex) val =
  case TI.lookup val progIndex of
    -- No constructors, val not permitted
    []  →  Nothing
    tys →  let foldActions accActions nextTy =
                  let actionMap = fromJust $ HMS.lookup nextTy progMap
                      actions = L.foldl' (++) [] (HMS.elems actionMap)
                  in  actions ++ accActions
            in  Just $ L.foldl' foldActions [] tys

 
