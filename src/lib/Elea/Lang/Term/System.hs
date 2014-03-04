


-- System related terms

-- TODO rough formalization of language
-- especially typing/logic features
--  and as state machine
--  and metaprogramming (which tie into typing)

data Constraints = Constraints
  { cnstrUnique ∷  [UniqConstraint]
  , cnstrParts  ∷  Type  
  }

data UniqConstraint = UniqConstraint [Lens]

bosonConstraints = Contraints [] Ty_None



-- Forces
----------------------------------------------------



data Effect = Effect
  { _effectPrec   ∷ Int
  , _effectForce  ∷ Force
  }



-- | Fundamental Forces
data Force =
    Force_Create  Cons_System
  | Force_Update  Cons_System


-- | Create action
data Cons_System = Cons_System
  { _consSysLoc ∷ Synthesis
  , _consSysMem ∷ Cons_Membrane
  , _consSysIni ∷ [Effect]
  }



-- System
----------------------------------------------------

data System = System
  { _sysMembrane  ∷ Membrane       -- | Exterior
  , _sysInterior  ∷ Interior       -- | Interior
  , _sysParent    ∷ System         -- | Location
  }


data Membrane = Membrane
  { _memVal     ∷ Val                -- | External structure
  , _memInter   ∷ Maybe Interaction  -- | External behavior
  , _memConsts  ∷ Contraints         -- | 'Selective Permeability'
  }


-- Subsystems are called particles
data Interior = Interior
  { _intCellMap ∷ TVar (HMS.HashMap Val (TVar System))
  , _intCellIdx ∷ TVar ValIndex
  }



-- System Constructors
----------------------------------------------------
data Cons_Membrane = Cons_Membrane
  { _valueCons ∷ Cons_Value
  , _interCons ∷ Maybe Cons_Interaction
  , _cnstrCons ∷ Cons_Constraints
  }


-- Synthesis should return 'Value
data Cons_Value = Cons_Value Synthesis


-- Synthesis should return 'Type'
data Cons_Interaction = Cons_Interaction
  { _consInterCause   ∷ Synthesis
  , _consInterEffects ∷ [Effect]
  }


data Cons_Constraints = Cons_Constraints
  { _consCnstrUnique  ∷ [Synthesis]
  , _consCnstrCells   ∷ Synthesis
  }



data Interaction = Interaction
  { interCause    ∷ Type
  , interEffects  ∷ [Effect]
  }


data SystemPath =
    Path_Absolute [Type]
  | Path_Relative [Type]
  | Path_Parent
  | Path_Here





