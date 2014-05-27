

-- | Types common to different term types.
--
-- This module is mostly to allow logical separation of
-- term types into reasonably-sized modules without
-- incurring issues with cyclical imports.
module Elea.Lang.Term.Identifiers where



import qualified Data.Text as T
import Data.UUID



type ForceId = T.Text

type SystemId = UUID

type ValueId = UUID

type ReceptorId = T.Text

type ActionPotentialId = T.Text


-- | A signal represents an occurrence of a value being
-- created which is described by the type of a receptor,
-- in which case the signal generated is equal to the 
-- identifier of that receptor.
type Signal = ReceptorId


