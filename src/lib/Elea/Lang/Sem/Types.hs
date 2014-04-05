

module Elea.Lang.Run.Types where



import Elea.Lang.Term.Transformer





--------------------- ACTION POTENTIAL ---------------------

-- | Stores the Action Potentials which accept a given signal
type SignalIndex = HMS.HashMap Signal [APId]


-- | An action potential may choose whether to accept signals.
-- An Inhibited AP loses all current events and ignores all
-- incoming signals.
data APState = Inhibited | Active


-- | Store 
type APMap = HMS.HasHMap APId (APState, SendTrans)



-------------------------- EVENT ---------------------------

type EventMap = HMS.HashMap Value (TVar Event)

type Event = HMS.HashMap Signal (Maybe Value)




------------------------ DATABASES -------------------------

data ReceptorDB =
  RecpDB
    TypeIndex
    (HMS.HashMap Type Receptor)


newReceptorDB ∷ ReceptorDB
newReceptorDB = RecpDB TI.newTypeIndex HMS.empty


data ParticleDB =
  PartDB
    ValueIndex
    (HMS.HashMap Value Particle)


newParticleDB ∷ ReceptorDB
newParticleDB = PartDB TI.newValueIndex HMS.empty



------------------------ UNIVERSE --------------------------


type SystemId = T.Text

type Universe = HMS.HashMap SystemId (TVar System)


data Reaction = Reaction ParamMap [ForceId]

newtype Action = Action Reaction

type ActionQueue = TQueue Action


data Program = Program (ForceId → Force)


data Context = Ctx ParamMap

-- An AP fired in some system


-- what does a program look like?
-- systems, action potentials, 
-- forces
-- libraries
-- import?
--  - forces?
--  - transformers?
--  - systems?


-- runtime vs semantics in this language



-- | Runtime Exceptions
data RuntimeException =
    SystemNotFound
  | DuplicateParticle




