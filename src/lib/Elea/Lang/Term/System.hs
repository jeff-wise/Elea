

module Elea.Lang.Term.System where



import Elea.Lang.Term.Transformer




------------------------ SYSTEM ---------------------------

type BroadcastTrans = Signal → Value → STM [Reaction]
type SendTrans = Signal → STM (Maybe Reaction)

type AddParticleTrans = Particle → STM ()

type TriggeredTrans = Value → STM [Signal]

type QueryTrans = Type → STM [Particle]


data SystemI =
  SysI
    BroadcastTrans
    AddParticleTrans
    TriggeredTrans
    QueryTrans




---------------------- FORCE INTERFACE --------------------

type SynthesizeTrans = Context → Synthesis → STM ()

type TransformTrans = Transformer → STM Value
type ProjectTrans = Projection → Value → STM ()

type EncodeTrans = Context → Encoding → STM ()


data ForceI =
  ForceI
    SynthesizeTrans
    EncodeTrans





--------------------- RUNTIME INTERFACE --------------------

-- | Runtime transactions
type AppendActionTrans = Action → STM ()
type FindSystemTrans = SystemId → STM System


data RuntimeI =
  RunI
    AppendActionTrans
    FindSystemTrans
    QueryTrans


-- | Runtime Exceptions
data RuntimeException =
    SystemNotFound
  | DuplicateParticle




--------------------------- FORCE -------------------------

type ForceId = T.Text



data Force =
    F_Syn Synthesis
  | F_Enc Encoding 
    

data Synthesis = Syn Transformer [Projection]

data Encoding = Enc SystemId RcptrId Cons_Type

data Projection = Projection Lens SystemId




---------------------- ACTION POTENTIAL --------------------

-- ***** Introductory Forms

type APId = T.Text
type EventClass = Lens
data EventInstance = Value


data Reaction = Reaction ParamMap [ForceId]

type ParamMap = HMS.HashMap Signal Value


data ActionPotential = AP EventClass [Signal] [ForceId]



-- ***** Evaluation Forms

type SignalIndex = HMS.HashMap Signal [APId]


data APState = Inhibited | Active

type APTransMap = HMS.HasHMap ApId (APSTate, Trans_ActionPotential)


type EventMap = HMS.HashMap Value (TVar Event)

type Event = HMS.HashMap Signal (Maybe Value)



----------------------- RECEPTOR  -------------------------

-- ***** Introductory Forms

type ReceptorId = T.Text
type Signal = RcptrId

data Receptor = Receptor ReceptorId Type



-- ***** Evaluation Forms

data ReceptorDB =
  RecpDB
    TypeIndex
    (HMS.HashMap Type Receptor)


newReceptorDB ∷ ReceptorDB
newReceptorDB = RecpDB TI.newTypeIndex HMS.empty




------------------------ PARTICLE --------------------------

-- ***** Evaluation Forms

newtype Particle = Part Value


data ParticleDB =
  PartDB
    ValueIndex
    (HMS.HashMap Value Particle)





------------------------- RUNTIME --------------------------

type SystemId = T.Text

type Universe = HMS.HashMap SystemId (TVar System)

type ActionQueue = TQueue Action


data Program = Program (ForceId → Force)


data Context = Ctx System ParamMap


-- An AP fired in some system
data Action = Action System Reaction




-- Crashes when force not present.
-- MUST be checked before added.
-- one of static properties
getForce ∷ ForceDict → ForceId → Maybe Force
getForce forceHM forceId = fromJust $ HMS.lookup forceId forceHM





handleError ∷ RuntimeException → STM a
handleError SystemNotFound    = return ()
handleError DuplicateParticle = return ()




-- what need to init?
--

init ∷ IO ()
init = do



