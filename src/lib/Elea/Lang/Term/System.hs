

module Elea.Lang.Term.System where


import Elea.Lang.Term.Basic
import Elea.Lang.Index.Value



---------------------------------------------------------------------
-- 1. System
---------------------------------------------------------------------

data System = System
  { _sysMembrane  ∷ Membrane
  , _sysInterior  ∷ Interior
  , _sysParent    ∷ System
  }


data Membrane = Membrane
  { _memValue ∷ Value
  , _memInter ∷ Interaction
  , _memCnstr ∷ Constraints
  }


data Interior = Interior
  { _intCellMap ∷ TVar (HMS.HashMap Value (TVar System))
  , _intCellIdx ∷ TVar ValueIndex
  }



data Interaction = Interaction
  { interCause  ∷ Cause
  , interEffect ∷ Effect
  }



data Constraints = Constraints
  { cnstrUnique ∷  [UniqConstraint]
  , cnstrCells  ∷  Type  
  }

data UniqConstraint = UniqConstraint [Lens]



---------------------------------------------------------------------
-- 1.2 System Constructors
---------------------------------------------------------------------

data Cons_System = Cons_System
  { _sysConsLoc ∷ URITy
  , _sysConsMem ∷ Cons_Membrane
  , _sysConsIni ∷ [Effect]
  }


data Cons_Membrane = Cons_Membrane




---------------------------------------------------------------------
-- 1.3 Utility Functions
---------------------------------------------------------------------

-- | Create an empty system interior
emptyInterior ∷ STM Interior
emptyInterior = Interior
            <$> newTVar HMS.empty
            <*> newTVar newValueIndex




---------------------------------------------------------------------
-- 2.0 Force
---------------------------------------------------------------------

data Force =
    Force_Create  SystemSource  
  | Force_Update  SystemSource
  | Force_Destroy Type                -- | URI Type
  | Force_IO      IOAction


data SystemSource =
    Src_Cons  Cons_System
  | Src_Lib   URI


data IOAction =
    IOPerform (IO ())
  | IORead  (IO String)


---------------------------------------------------------------------
-- 2.1 Cause & Effects of Forces
---------------------------------------------------------------------

type Trigger = Value

-- | Cause
-- A system event.
data Cause =
    -- | The system reached a threshold number
    -- of cells of a specified 'Type'
    -- The 'Trigger' is a set of values of the cells
    -- of the specified type
    Threshold Int Type
    -- | A new cell was added to the system.
    -- The 'Trigger' is the new cell's value
  | NewCell Type
--  | Relational Int Type
  | Rejection



data Effect = Effect
  { _effectPrec   ∷ Int 
  , _effectForces ∷ [Force]
  }



