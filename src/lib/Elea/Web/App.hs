

module Elea.Web.App where


import Elea.Lang.Types

import qualified Data.HashMap.Strict as HMS



liftWeb ∷ App → [SystemId] → IO ()
liftWeb (App name univ) systemIds = do



systemHandlers ∷ Universe → [SystemId] → HMS.HashMap SystemId (ScottyM ())
systemHandlers univ systemIds =
  foldL' addHandlerfor systemIds (\sytemId → 
          systemWebHandler systemId
      )
  where
    addHandler handlerMap systemId = 
      let system = lookupSystem systemId univ
      in  HMS.insert systemId (systemWebHandler systemId) handlerMap

