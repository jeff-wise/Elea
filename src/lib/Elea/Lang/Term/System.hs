

module Elea.Lang.Term.System
  ( -- * Force
    -- * Action Potential
    -- * Receptor
    -- * Particle

  ) where



import Elea.Lang.Term.Transformer




--------------------------- FORCE -------------------------

-- | Force
data Force =
    F_Syn Synthesis
  | F_Enc Encoding 


type ForceId = T.Text

    

data Synthesis = Syn Transformer [Projection]

data Encoding = Enc SystemId RcptrId Cons_Type

data Projection = Projection Lens SystemId




---------------------- ACTION POTENTIAL --------------------

type APId = T.Text
type EventClass = Lens
data EventInstance = Value


data Reaction = Reaction ParamMap [ForceId]

type ParamMap = HMS.HashMap Signal Value


data ActionPotential = AP EventClass [Signal] [ForceId]


-- | An action potential may choose whether to accept signals.
-- An Inhibited AP loses all current events and ignores all
-- incoming signals.
data APState = Inhibited | Active




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


-- TODO map multiple signals to type any



handleError ∷ RuntimeException → STM a
handleError SystemNotFound    = return ()
handleError DuplicateParticle = return ()




-- what need to init?
--

init ∷ IO ()
init = do



