

module Elea.Lang.Sys.Types where


import Elea.Prelude
import Elea.Lang.Atom.Lens
import Elea.Lang.Atom.Types
import Elea.Lang.Index.Type
import Elea.Lang.Index.Val


import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import GHC.Generics (Generic)




---------------------------------------------------------------------
-- Action
---------------------------------------------------------------------


type Event = [Action]

type EventMap = HMS.HashMap Type Event


data Action = 
    Spawn (System → Particle)
  | Synth Synthesis
  | Proc Val [(Val, Val)]
  | Send Type Particle




data Reactor = Reactor
  { _evMap   ∷ TVar (HMS.HashMap Type Event)
  , _evIndex ∷ TVar TI.TypeIndex
  }





---------------------------------------------------------------------
-- System
---------------------------------------------------------------------


type Env = HMS.HashMap Val Val

data System = System 
  { _sysId        ∷  Val
  , _sysVal       ∷  TVar Val
  , _sysEnv       ∷  TVar Env
  , _sysSymTable  ∷  TVar SymbolTable
  , _sysPartMap   ∷  TVar (HMS.HashMap ParticleId (Seq.Seq Particle))
  , _sysPartIndex ∷  TVar ValIndex
  , _sysProgram   ∷  TVar Program
  , _sysRelMap    ∷  TVar RelMap
  }


newSystem ∷ Val → Env → STM System
newSystem systemId env =
  System
    <$> (return systemId)
    <*> (newTVar Val_Null)
    <*> (newTVar env)
    <*> (newTVar newSymbolTable)
    <*> (newTVar HMS.empty)
    <*> (newTVar newValIndex)
    <*> (newProgram >>= newTVar)
    <*> (newTVar HMS.empty)



type ParticleId = Val
type ParticleKey = Int


data Particle = Particle
  { _partId   ∷  ParticleId
  , _partVal  ∷  Val
  }
  deriving (Eq, Generic)


instance Hashable Particle

instance Show Particle where
  show (Particle ident val) = 
    "Particle\n" ++ show ident ++ "\n"
                 ++ show val  ++ "\n"





data Program = Program 
  { _spawnReactor ∷ TVar Reactor
  , _recvReactor  ∷ TVar Reactor
  }




newProgram :: STM Program
newProgram =
  Program
    <$> (newTVar $ OnInit [])
    <*> (newTVar $ OnRecv HMS.empty newTypeIndex)
    <*> (newTVar $ OnSpawn HMS.empty newTypeIndex)






data SymbolTable = SymTable
  { _tblCount ∷  Int
  , _tblMap   ∷  HMS.HashMap Int T.Text
  }


newSymbolTable ∷ SymbolTable
newSymbolTable = SymTable 0 HMS.empty





---------------------------------------------------------------------
-- Universe
---------------------------------------------------------------------


-- Top level system
-- Contains built-in systems
data Universe = Universe
  { _library    ∷  TVar Library
  , _procMap    ∷  TVar (HMS.HashMap Val System)
  , _sysReactor ∷  TVar Reactor
  , _sysIndex   ∷  VI.Index
  }


newUniverse ∷ STM Universe
newUniverse =
  Universe
    <$> (newTVar $ Library HMS.empty HMS.empty)
    <*> (newTVar HMS.empty)


-- | Library
-- will try to find systems and particles not
-- defined on the internet at runtime
data Library = Library
  { _libSysMap  ∷ HMS.HashMap Val System
  , _libPartMap ∷ HMS.HashMap Val Particle  
  }


                  



---------------------------------------------------------------------
-- Synthesis
---------------------------------------------------------------------

data Synthesis = Syn
  { _synName  ∷  T.Text
  , _synCons  ∷  [Constructor]
  , _synApps  ∷  Seq.Seq Application
  , _synPartT ∷  Particle
  }


data Application = App
  { _appName    ∷  Maybe Text
  , _appParams  ∷  [Param]
  , _appFunName ∷  T.Text       -- TODO make this datatype?
  , _resultId   ∷  Val
  }      


data Param = Param
  { _colIndex   ∷  Int
  , _paramLens  ∷  Lens
  } 






-----------------------------------------------------------
-- Constructor
-----------------------------------------------------------



-- | Uniquely identify a relation
data RelDef = RelDef
  { _relName  ∷  Text  -- | Name of relation
  , _domain   ∷  Text  -- | Name of domain set
  , _codomain ∷  Text  -- | Name of codomain set
  }



type ParticleIdSet = HS.HashSet ParticleId

-- | Mathematical relation: domain X codomain
type Relation = HMS.HashMap ParticleId ParticleIdSet


-- | Store relations efficiently
type RelMap = HMS.HashMap RelDef Relation


-- reason to use particle key
--
-- quick lookup of specific particles
-- don't store version in index



data Query = 
  Query
    -- | Match ParticleIDs 
    Matcher
    -- | Filter matched particles by value
    Type



data Matcher = 
    MatchType Type
  | MatchRel  Relation



type SystemId = Val



data Constructor = 
    Con_Val Val
  | Con_This -- value of this system
  | Con_Run SystemId [(Val, Val)]
 -- | Con_Query Query
  -- TODO Generator...how/what...constraints?



-----------------------------------------------------------
-- System Modifiers
-----------------------------------------------------------





makeLenses ''System
makeLenses ''SymbolTable
makeLenses ''Program
makeLenses ''OnSpawn
makeLenses ''OnRecv
makeLenses ''Library
