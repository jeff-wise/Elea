

module Elea.Lang.Exp.Types where


import Elea.Prelude
import Elea.Lang.Atom.Lens
import Elea.Lang.Atom.Types
import qualified Elea.Lang.Atom.Index.Type as TI
import Elea.Lang.Atom.Index.Val


import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T




data Context = Ctx
  { _universe   ∷  Universe
  , _currSysVar ∷  TVar System
  , _trigSysVal ∷  Val
  }



---------------------------------------------------------------------
-- Universe
---------------------------------------------------------------------

data Universe = Universe
  { _mainSys  ∷  TVar System
  , _symTable ∷  TVar SymbolTable
  }


data SymbolTable = SymTable
  { _tblCount ∷  Int
  , _tblMap   ∷  HMS.HashMap Int T.Text
  }


newSymbolTable ∷ SymbolTable
newSymbolTable = SymTable 0 HMS.empty





---------------------------------------------------------------------
-- System
---------------------------------------------------------------------


-- | System
data System = System 
  { _sysVal           ∷  Val
  , _sysCatalyst      ∷  Catalyst
  , _sysParent        ∷  TVar System
  , _sysCounter       ∷  TVar Int
  }


data System = System
  { _sysVal         ∷  Val
  , _sysCatalyst    ∷  Catalyst
  , _sysParent      ∷  TVar System
  , _sysField       ∷  Field
  }

data Field = Field
  { _partMap    ∷ TVar (HMS.HashMap Val (TVar System))
  , _partValIdx ∷ TVar ValIndex
  , _idCounter  ∷ TVar Int
  }



-- | Create action
data Create = Create
  { _creSysVal    ∷ Val 
  , _creSysForce  ∷ Force
  , _creSysParts  ∷ [System]
  }


data Force = Force
  { _trigger  ∷  Type
  , _reaction ∷  Reaction
  }


type Reaction = [(Priority, Action)]




--  { _synSysCons ∷  SysConstructor
--  , _synSysLoc  ∷  SysPath
--  }

data Destroy = Destroy SysPath

data SysPath =
    Absolute [Type]
  | Relative [Type]
  | ParentSys
  | ThisSys



data SysConstructor = SysCons
  { _defVal       ∷  Synthesis
  , _defCat       ∷  Catalyst
  , _defChildren  ∷  [SysConstructor]
  }



-- where to find values
-- whenever create is called...
--  check values (synthesis) of created systems
--     and subsystems 
-- wait untiil create is *called* though
-- Context state monad...?

-- create universe
-- and call initial actions on it


data Synthesis = Synthesis
  { _queries ∷ [Query]
  , _funTree ∷ FunTree
  }



data Reaction = Reaction [ActionGroup]


-- | Root function, returns val (not named val)
data Fun = Fun Function [FunTree]

-- | Function tree, evaluates to a named val
data FunTree =
    Fetch Val
  | Apply Val Function [FunTree]



data NamedVal = NamedVal Val Val

type Env = HMS.HashMap Val Val


data Function =
    ValT  Val
  | Prim  T.Text
 -- | HaskFun HaskellFunction
 -- | EqFun   Equation   
 -- | 


evalFunTree ∷ 



-- | Create a new system
newSystemVar ∷ Program → Val → Maybe (TVar System)
               → STM (TVar System)
newSystemVar prog systemVal parentSysVar =
  let newSystem = System
                    <$> (return systemVal)
                    <*> (newTVar HMS.empty)
                    <*> (newTVar newValIndex)
                    <*> (newTVar prog)
                    <*> (newTVar HMS.empty)
                    <*> (return parentSysVar)
  in  newSystem >>= newTVar

 

type Relation = HMS.HashMap Val (HS.HashSet Val)

-- TODO way to pretty-print arbitrary values?
type RelationId = Val

type RelMap = HMS.HashMap RelationId Relation




---------------------------------------------------------------------
-- Program
---------------------------------------------------------------------

data Constructor = Con
  { _progRuleMap  ∷ HMS.HashMap Type ActionMap
  , _progRuleIdx  ∷ TI.TypeIndex
  }



type ActionMap = HMS.HashMap Val [Action]


newProgram ∷ Constructor
newProgram = Con HMS.empty TI.newTypeIndex


                

data ActionDef = ActionDef
  { _actId    ∷  Val
  , _actPrior ∷  Int
  , 

data Action = Action_Con Constructor




data Map = Map
  { _mapSyn     ∷ Synthesis Type
  , _mapRelDefs ∷ [RelDef]
  }



-- | What occurs when a system is created
data ConsEvent = ConsEvent
  { _initNewSysThrds  ∷ [IO ()]
  , _parSysThrds      ∷ [IO ()]
  }



---------------------------------------------------------------------
-- Synthesis
---------------------------------------------------------------------

-- | Transformers modify data using different methods
data Transformer =
    Tr_Const
  | Tr_Query Type
  | Tr_ValTemplate
--  | Tr_Equation
--  | Tr_Text


data Query = Query


-- | Find values elsewhere in the system
data From =
    From_Const Val
  | From_Ctx
  | From_Type  Type
 --  | Query_Rel  RelQuery -- can do as type query??
  | From_Trig



-- how to make types from values
-- special type constructor transformer



data RelDef = RelDef Type Type



-- Lenses 
makeLenses ''Context
makeLenses ''Constructor
makeLenses ''System
makeLenses ''Synthesis
makeLenses ''SymbolTable
makeLenses ''Program
makeLenses ''Universe




-- Utility functions


