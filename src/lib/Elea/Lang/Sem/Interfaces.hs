

module Elea.Lang.Run.Interfaces where



import Elea.Lang.Term.Transformer




------------------------ SYSTEM ---------------------------

type BroadcastTrans = Signal → Value → STM [Reaction]
type SendTrans = Signal → STM (Maybe Reaction)

type AddParticleTrans = Particle → STM ()

type TriggeredTrans = Value → STM [Signal]



data SystemI =
  SysI
    BroadcastTrans
    AddParticleTrans
    AddActionPotentialTrans
    TriggeredTrans




---------------------- FORCE INTERFACE --------------------

type SynthesizeTrans = Context → Synthesis → STM ()

type TransformTrans = Transformer → STM Value
type ProjectTrans = Projection → Value → STM ()

type EncodeTrans = Context → Encoding → STM ()

type IOActionTrans = IOAction → STM ()


data IOAction =
    IOPerform (IO ())
--  | IORequest (IO T.Text)


data ForceI =
  ForceI
    SynthesizeTrans
    EncodeTrans
    IOActionTrans





--------------------- RUNTIME INTERFACE --------------------

-- | Runtime transactions
type AppendActionTrans = Action → STM ()
type FindSystemTrans = SystemId → STM System
type QueryTrans = SystemId → Type → STM [Particle]


data RuntimeI =
  RunI
    AppendActionTrans
    FindSystemTrans
    QueryTrans



