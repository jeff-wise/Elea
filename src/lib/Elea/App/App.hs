

module Elea.App.App where


import Elea.Lang.Types


import qualified Data.Text as T


data App = App
  { appName ∷  T.Text 
  , appUniv ∷  Universe
  }
