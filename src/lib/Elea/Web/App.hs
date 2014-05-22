

module Elea.Web.App where


import Elea.Lang.Types

import qualified Data.HashMap.Strict as HMS



liftWeb ∷ App → [SystemId] → IO ()
liftWeb (App name univ) systemIds = do



systemHandlers ∷ [SystemId] → HMS.HashMap SystemId (ScottyM ())
systemHandlers systemIds =
  foldL' addHandlerfor systemIds (\sytemId → 
          systemWebHandler systemId
      )
  where
    addHandler handlerMap systemId = 
      let system = lookupSystem systemId (univ

lookupSystem ∷ SystemId → SystemMap → System
      in  HMS.insert systemId (systemWebHandler systemId) handlerMap
