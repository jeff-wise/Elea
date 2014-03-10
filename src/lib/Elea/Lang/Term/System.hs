

module Elea.Lang.Term.System
  ( -- * System
    --   $systemDesc
    System (..)
  , Membrane (..), Interior (..)
  , Interaction (..)
  , Constraints (..), UniqConstraint (..), CellConstraint (..)
    -- ** System Constructors
  , Cons_System (..)
  , Cons_Membrane (..)
    -- *** Membrane Constructors
  , Cons_Value (..), Cons_Interaction (..), Cons_Constraints (..)
    -- ** System Utilities
  , emptyInterior, particle, free
    -- * Force
    -- * Cause/Effects
  ) where


import Elea.Lang.Term.Basic
import Elea.Lang.Index.Value


import qualified Data.PQueue.Min as PQ



---------------------------------------------------------------------
-- 1. System
---------------------------------------------------------------------



-- | System
data System = System Membrane Interior


-- | System Membrane
-- Parameters
--  * Value - 
data Membrane =
  Membrane Value (Maybe Interaction) [Constraint]



data Interior =
  Interior
    (TVar (HMS.HashMap Value (TVar System)))
    (TVar ValueIndex)



data Interaction = Interaction Cause EffectQueue


data Constraint = 
    Cnstr_Unique [Lens]
  | Cnstr_CellTy Type



data Location =
    Absolute [Type]
  | Relative [Type]
  | Parent
  | Here




---------------------------------------------------------------------
-- 1.2 System Constructors
---------------------------------------------------------------------


-- | Membrane Constructor
--  * Membrane Constructor (Cons_Membrane) - How to build the
--      system's membrane, which gives the system most
--      of its important properties.
data Cons_Membrane =
  Cons_Membrane
    Cons_Value
    (Maybe Cons_Interaction)
    Cons_Constraints



-- | Value Constructor
-- The value of the membrane is a function of the
-- current state of the system, which has no side effects.
newtype Cons_Value = Cons_Value Synthesis



-- | Interaction Constructor
data Cons_Interaction =
  Cons_Interaction Cons_Cause EffectQueue



-- | Constraints Constructor
data Cons_Constraints = Cons_Constraints [Cons_Constraint]

data Cons_Constraint = 
    Cnstr_Unique [Lens]
  | Cnstr_CellTy Type






---------------------------------------------------------------------
-- 1.3 Utility Functions
---------------------------------------------------------------------

-- | Create an empty system interior
emptyInterior ∷ STM Interior
emptyInterior = Interior
            <$> newTVar HMS.empty
            <*> newTVar newValueIndex



-- | A particle is a system with no subsystems (cells)
particle ∷ Cons_Constraints
particle = Cons_Constraints [] (Syn.tyConst Ty_None)


-- | A Constraints constructor which defines a free system.
-- A free system has no contraints.
free ∷ Cons_Constraints
free = Cons_Constraints [] (Syn.tyConst Ty_Any)



---------------------------------------------------------------------
-- 2.0 Force
---------------------------------------------------------------------

data Force =
    Force_Create  Location Cons_Membrane
  | Force_Update  Location Type Cons_Membrane
  | Force_Destroy Location
  | Force_IO      IOAction



data IOAction =
    IOPerform (Value → IO ())
  | IORead  (Value → IO Text) EffectQueue




---------------------------------------------------------------------
-- 3.0 Cause & Effects
---------------------------------------------------------------------

type Trigger = Value


-- | Cause
-- A system event.
data Cause =
    -- | The system reached a threshold number
    -- of cells of a specified 'Type'
    -- The 'Trigger' is a set of values of the cells
    -- of the specified type
    OnThreshold Int Type
    -- | A new cell was added to the system.
    -- The 'Trigger' is the new cell's value
  | OnNewCell Type
  | OnRejection



data Effect = Effect Int Force
 
type EffectQueue = PQ.MinQueue Effect


---------------------------------------------------------------------
-- 3.1 Cause/Effects Constructors
---------------------------------------------------------------------

data Cons_Cause = 
    Cons_OnThreshold Synthesis Synthesis
  | Cons_OnNewCell Synthesis
  | Cons_OnRejection



---------------------------------------------------------------------
-- 3.3 Ordered Effects
---------------------------------------------------------------------

instance Ord Effect where
  compare (Effect i _) (Effect j _) = i `compare` j





---------------------------------------------------------------------
-- 3.2 Cause/Effects Utilities
---------------------------------------------------------------------

effectQueue ∷ [Effect] → EffectQueue
effectQueue = L.foldl' (flip PQ.insert) PQ.empty


