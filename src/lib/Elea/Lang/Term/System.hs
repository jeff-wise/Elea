

module Elea.Lang.Term.System where



import Elea.Lang.Term.Transformer


------------------------ SYSTEM ---------------------------

type BroadcastTrans = Signal → Value → STM [Reaction]
type SendTrans = Signal → STM (Maybe Reaction)

type AddParticleTrans = Particle → STM ()

type TriggeredTrans = Value → STM [Signal]


data System =
  Sys
    BroadcastTrans
    AddParticleTrans
    TriggeredTrans



---------------------- FORCE INTERFACE --------------------


type SynthesizeTrans = Context → Synthesis → STM ()

type TransformTrans = Transformer → STM Value
type ProjectTrans = Projection → Value → STM ()

type EncodeTrans = Context → Encoding → STM ()


data ForceI =
  ForceI SynthesizeTrans EncodeTrans





--------------------- RUNTIME INTERFACE --------------------

-- | Runtime transactions
type AppendActionTrans = Action → STM ()
type FindSystemTrans
type QueryTrans


data RuntimeI =
  RunI
    AppendActionTrans
    FindSystemTrans
    QueryTrans


-- | Runtime Exceptions
data RuntimeException =
    SystemNotFound
  | DuplicateParticle





-- Action
-----------------------------------------------------------

-- ***** Def Forms *****

data Projection = Projection Lens SystemId

type ForceId = T.Text


data Force =
    F_Syn Synthesis
  | F_Enc Encoding 
    

data Synthesis = Syn Transformer [Projection]

data Encoding = Enc SystemId RcptrId Cons_Type


--------------------- ACTION POTENTIAL --------------------


-- ***** Introductory Forms

type ActnPotlId = T.Text
type EventClass = Lens
data EventInstance = Value


data Reaction = Reaction ParamMap [ForceId]

type ParamMap = HMS.HashMap Signal Value


data ActionPotential = AP EventClass [Signal] [ForceId]


data APState = Inhibited | Active



-- ***** Evaluation Forms

type SignalIndex = HMS.HashMap Signal [APId]

-- event occurs per AP
-- event is triggering of action
-- so signal goes to some action, but one action
--   could have many instances simultaneously occurring
--   so must map signal to event, using event class
-- rule: each signal can can only cause one reaction per AP
type APTransMap = HMS.HasHMap ApId (APSTate, Trans_ActionPotential)

-- because STM works on vars, can define functions with
-- fixed structure. do not need state function


type Event = HMS.HashMap Signal (Maybe Value)

type EventMap = HMS.HashMap Value (TVar Event)




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

newtype Particle = Part Value

data ParticleDB =
  PartDB
    ValueIndex
    (HMS.HashMap Value Particle)



type SystemId = T.Text


type Universe = HMS.HashMap SystemId (TVar System)

type ActionQueue = TQueue Action






data Program =
  Program
    (ForceId → Force)



data Context = Ctx System ParamMap


-- An AP fired in some system
data Action = Action System Reaction



-- always write new trans
-- sometimes put in default vals
-- sometimes recv them
-- add typeclass?


-- interp vs compile
-- forceId resolution


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







