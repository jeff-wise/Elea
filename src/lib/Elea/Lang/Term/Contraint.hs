


-- System related terms



data Constraint = Constraint
  { consTy    ∷ ConstraintType
  , consValTy ∷ Type
  }



data ConstraintType =
    Unique
