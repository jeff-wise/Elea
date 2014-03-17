

module Elea.Lang.Term.System where


import Elea.Lang.Term.Basic
import Elea.Lang.Index.Value




data System = 
  System
    Value
    (Maybe Interaction)
    (Maybe Interior)
    (HMS.HashMap T.Text ActionPotential)



data Interaction =
  Interaction Cause [Force]



data Interior =
  Interior
    [Constraint]
    Collection


data Collection =
  Collection
    (TVar (HMS.HashMap Value (TVar System)))
    (TVar ValueIndex)



data ActionPotential = ActionPotential (TVar (HMS.HashMap T.Text Bool))


data Force = F_System Cons_System



data Cons_System =
  Cons_System
    Cons_Value
    Cons_Interaction
    Cons_Interior


data Cons_Value = Cons_Value Function [Projection]

data Cons_Interaction = Cons_Interaction Cons_Cause [Force]

data Cons_Interior = Cons_Interior [Cons_Constraint]




data Charge = Charge T.Text T.Text


data Path = Path (Maybe UniversePath) SystemPath


data UniversePath =
    Web URI
  | Lib T.Text







data Cons_Cause = Cons_Activation T.Text


data Cause =
    Activation T.Text
  | Addition Type
  | Rejection


data Cons_Constraint = 
    Cons_Unique [Lens]
  | Cons_CellTy SystemPath


data Constraint = 
    Unique [Lens]
  | CellTy Type








----------------------------------------------------------

type Signal = T.Text
type ReceptorName = T.Text
type CellName = T.Text
type ReceptorShape = Type


data ActionPotential = ActionPotential Signal [Force]


newtype Cell = Cell [Receptor]

data Receptor = Receptor ReceptorName ReceptorShape


data System = System Value (HMS.HashMap T.Text Cell)


data Force = Create Function Projection


data Projection = Projection Lens Path Type


data Path = Path SystemPath CellName



data SystemPath =
    Absolute [Type] 
  | Relative [Type]
  | Parent
  | Here




-- Need to mark things as FINAL
-- rename dict to record
