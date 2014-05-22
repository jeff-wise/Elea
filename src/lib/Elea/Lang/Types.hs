

-- | Language Types
--
-- Coalesce external language modules for easier export
-- These are the modules we want other modules such as plugins
-- or standard libraries or ancillary modules (such as the Web module)
-- to use for each access to the language types and functions.
--
-- The cabal library will probably still export every module just
-- in case they are needed.
module Elea.Lang.Types (
    -- * Terms
    module Elea.Lang.Term.Force
  , module Elea.Lang.Term.Identifiers
  , module Elea.Lang.Term.Lens
  , module Elea.Lang.Term.System
  , module Elea.Lang.Term.Transformer
  , module Elea.Lang.Term.Type
  , module Elea.Lang.Term.Value
    -- * Semantics
  , module Elea.Lang.Sem.Processor
  , module Elea.Lang.Sem.System
  , module Elea.Lang.Sem.Types
  ) where



import Elea.Lang.Sem.Processor
import Elea.Lang.Sem.System
import Elea.Lang.Sem.Types

import Elea.Lang.Term.Force
import Elea.Lang.Term.Identifiers
import Elea.Lang.Term.Lens
import Elea.Lang.Term.System
import Elea.Lang.Term.Transformer
import Elea.Lang.Term.Type
import Elea.Lang.Term.Value


