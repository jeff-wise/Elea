

module Elea.App.App where


import Elea.Lang.Types


import qualified Data.Text as T


type AppId      = T.Text
type AppIdShort = T.Text


data App = App
  { appId       ∷  AppId
  , appIdShort  ∷  AppIdShort
  , appUniv     ∷  Universe
  }
