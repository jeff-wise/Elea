
{-# LANGUAGE DeriveGeneric #-}


---------------------------------------------------------------------
-- | 
-- Module with all of the main types.
--
-- Necessary to avoid circular imports
---------------------------------------------------------------------
module Elea.Lang.Types 
  ( Val (..)
  , Set (..), Array (..), pair
  , Text (..), Number (..), Symbol, newSymbol
  , DateTime (..), Variable (..)
  , Error (..)
  , SynError (..), AppError (..), ParamError (..)
  , Type (..)
  , SetTy (..), ArrayTy (..)
  , pairTy, fstTy, sndTy
  , AndTy (..), OrTy (..)
  , TextTy (..), NumberTy (..), SymbolTy (..)
  , DateTimeTy (..), VariableTy (..)
  , Env, SymbolTable (..), newSymbolTable
  , System (..), Particle (..)
  , Synthesis (..), Application (..), Param (..)
  , Lens (..)
  , getFromFst, getFromSnd, getFromBoth, atIndex
  , SetLens (..), ArrayLens (..)
  , Constructor (..)
  , FunDef (..), FunDict
  -- * Lenses
  , tblCount, tblMap
  , sysVal, sysEnv, symTable
  ) where



import Elea.Prelude


import Control.Monad.State.Lazy

import qualified Data.HashMap.Strict as HMS
import qualified Data.List.Stream as L
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import GHC.Generics (Generic)




---------------------------------------------------------------------
-- Elea Values
---------------------------------------------------------------------

data Val = 
    Val_Set     Set
  | Val_Arr     Array
  | Val_Text    Text
  | Val_Num     Number
  | Val_Sym     Symbol
  | Val_Dtm     DateTime
  | Val_Var     Variable
  | Val_Err     Error
  deriving (Eq, Generic)

instance Hashable Val

instance Show Val where
  show (Val_Set  set ) = show set
  show (Val_Arr  arr ) = show arr
  show (Val_Text text) = show text
  show (Val_Num  num ) = show num
  show (Val_Sym  sym ) = show sym
  show (Val_Dtm  dtm ) = show dtm
  show (Val_Var  var ) = show var
  show (Val_Err  err ) = show err



data Set = Set
  { _getSet ∷ HashSet Val } 
  deriving (Eq, Generic)

instance Hashable Set

instance Show Set where
  show (Set hs) = show hs



data Array = Arr
  { _getArr ∷  Seq.Seq Val }
  deriving (Eq, Generic)

instance Hashable Array

instance Show Array where
  show (Arr arr) = show arr


-- Convenience function to create a
-- 2-element array
pair ∷ Val → Val → Val
pair val1 val2 = Val_Arr $ Arr (val1 <| val2 <| Seq.empty)





data Text = Text 
  { _getText ∷ T.Text }
  deriving (Eq, Generic)

instance Hashable Text

instance Show Text where
  show (Text text) = show text



-- Internally either int or float
-- This makes dealing with numbers conceptually easier
-- without worrying about hardware/performance details.
-- The compiler is simple enough I think that later
-- optimization should be straightfoward in most cases.
data Number = 
    Z Int 
  | R Double
  deriving (Generic)


instance Eq Number where
  (==) (Z x) (Z y) = x == y
  (==) (Z i) (R d) = fromIntegral i == d
  (==) (R x) (R y) = x == y
  (==) (R d) (Z i) = d == fromIntegral i


instance Ord Number where
  compare (Z x) (Z y) = compare x y
  compare (R x) (R y) = compare x y
  compare (Z i) (R d) = compare (fromIntegral i) d
  compare (R d) (Z i) = compare d (fromIntegral i)


instance Num Number where

  (+) (Z x) (Z y) = Z $ x + y
  (+) (Z x) (R y) = R $ fromIntegral x + y
  (+) (R x) (Z y) = R $ x + fromIntegral y
  (+) (R x) (R y) = R $ x + y

  (-) (Z x) (Z y) = Z $ x + y
  (-) (Z x) (R y) = R $ fromIntegral x + y
  (-) (R x) (Z y) = R $ x + fromIntegral y
  (-) (R x) (R y) = R $ x + y
  
  (*) (Z x) (Z y) = Z $ x + y
  (*) (Z x) (R y) = R $ fromIntegral x + y
  (*) (R x) (Z y) = R $ x + fromIntegral y
  (*) (R x) (R y) = R $ x + y

  negate (Z i) = Z $ negate i
  negate (R d) = R $ negate d

  abs (Z i) = Z $ abs i
  abs (R d) = R $ abs d

  signum (Z i) = Z $ signum i
  signum (R d) = R $ signum d

  fromInteger = Z . fromInteger



instance Hashable Number

instance Show Number where
  show (Z i) = show i
  show (R d) = show d


data Symbol = Sym Int
  deriving (Eq, Generic)

instance Hashable Symbol

instance Show Symbol where
  show (Sym symId) = "Symbol " ++ show symId



newSymbol ∷ T.Text → State System Symbol
newSymbol symName = state $ \system →
  let (SymTable count symMap) = _symTable system
  in  case L.find ((symName==) . snd) $ HMS.toList symMap of
        -- Symbol already defined, return it
        Just (k, _) → (Sym k, system)
        -- Create a new int mapping for text symbol
        Nothing     →
          let count' = count + 1
              system' = system {
                -- Update symbol table
                _symTable = SymTable {
                    _tblCount  = count'
                  , _tblMap    = HMS.insert count' symName symMap
                }
              }
          in  (Sym count', system')








data DateTime = DateTime 
  { _date ∷  Day
  , _time ∷  TimeOfDay
  } deriving (Eq, Generic)

instance Hashable DateTime

instance Show DateTime where
  show (DateTime date time) =
    show date ++ " " ++ show time


data Variable = Var
  { _varName  ∷  Val }
  deriving (Eq, Generic)

instance Hashable Variable

instance Show Variable where
  show (Var name) = "Variable: " ++ show name



-- Error Values

data Error = Err_Syn Text SynError
  deriving (Eq, Generic)

instance Hashable Error

instance Show Error where
  show (Err_Syn synName err) =
    "Error in Synthesis " ++ show synName
    ++ "\nError:\n" ++ show err




-- | Synthesis Error
data SynError = 
  SynAppError Number AppError
  deriving (Eq, Generic)

instance Hashable SynError

instance Show SynError where
  show (SynAppError num err) = 
    "Application #" ++ show num
    ++ "\nError:\n" ++ show err



data AppError = 
    AppParamError [Either ParamError Val]
  | AppFunNotFound
  | ExecError   -- TODO this should never occur??
  deriving (Eq, Generic)

instance Hashable AppError

instance Show AppError where
  show (AppParamError xs) = 
    "An error occurred in an application parameter\n" ++
    "Parameters:\n" ++ show xs
  show AppFunNotFound     = "Function is not defined"
  show ExecError          = "Execution Error"



data ParamError = 
    ParamLensNotFound 
  | IncorrectParamType Type Val
  | RefColDoesNotExist
  deriving (Eq, Generic)

instance Hashable ParamError

instance Show ParamError where
  show ParamLensNotFound  = 
    "The specified value does not exist in the referenced column"
  show (IncorrectParamType ty val) = 
    "The given parameter:\n" ++ show val ++ "\n" ++
    "should be of type:\n" ++ show ty
  show RefColDoesNotExist = 
    "Column referenced in synthesis has not been defined" 


---------------------------------------------------------------------
-- Elea Types
---------------------------------------------------------------------

data Type = 
    Ty_Set    SetTy
  -- | Ty_Pair   PairTy
  | Ty_Arr    ArrayTy
  | Ty_And    AndTy
  | Ty_Or     OrTy
  | Ty_Text   TextTy
  | Ty_Num    NumberTy
  | Ty_Sym    SymbolTy
  | Ty_Dtm    DateTimeTy
  | Ty_Var    VariableTy
  | Ty_Any 
  deriving (Eq, Generic)

instance Hashable Type


instance Show Type where
  show (Ty_Set  setTy ) = show setTy
  -- show (Ty_Pair pairTy) = show pairTy
  show (Ty_Arr  arrTy ) = show arrTy
  show (Ty_And  andTy ) = show andTy
  show (Ty_Or   orTy  ) = show orTy
  show (Ty_Text textTy) = show textTy
  show (Ty_Num  numTy ) = show numTy
  show (Ty_Sym  symTy ) = show symTy
  show (Ty_Dtm  dtmTy ) = show dtmTy
  show (Ty_Var  varTy ) = show varTy
  show  Ty_Any          = "Any"



data SetTy = 
    WithElem      Type
--  | WithoutElem   Type
--  | IsSet         [Type]
  | SetWithSize   Number
  | AnySet
  deriving (Eq, Generic)

instance Hashable SetTy

instance Show SetTy where
  show (WithElem ty) = 
    "WithElem of " ++ show ty
  show (SetWithSize size) = 
    "Set with size of " ++ show size
 -- show (IsSet set) = "Is " ++ show set
  show  AnySet       = "Set"


-- TODO array with size
data ArrayTy = 
    IsArray    (Seq.Seq Type)
  | WithIndex  Number Type 
  | AnyArray
  deriving (Eq, Generic)

instance Hashable ArrayTy

instance Show ArrayTy where
  show (IsArray arr)    = "Is " ++ show arr
  show (WithIndex i ty) = "At " ++ show i 
    ++ " type of " ++ show ty
  show AnyArray         = "Array"



pairTy ∷ Type → Type → Type
pairTy ty1 ty2 = Ty_Arr $ IsArray (ty1 <| ty2 <| Seq.empty)


fstTy ∷ Type → Type
fstTy ty = Ty_Arr $ WithIndex (Z 0) ty


sndTy ∷ Type → Type
sndTy ty = Ty_Arr $ WithIndex (Z 1) ty






data AndTy = 
  AndTy Type Type
  deriving (Eq, Generic)
 
instance Hashable AndTy

instance Show AndTy where
  show (AndTy ty1 ty2) = 
    show ty1 ++ "\nAND\n" ++ show ty2


data OrTy = 
  OrTy  Type Type
  deriving (Eq, Generic)

instance Hashable OrTy

instance Show OrTy where
  show (OrTy ty1 ty2) = 
    show ty1 ++ "\nOR\n" ++ show ty2


data TextTy = 
    WithTextLen  Number
  | IsText       Text
  | AnyText
  deriving (Eq, Generic)

instance Hashable TextTy

instance Show TextTy where
  show (WithTextLen len ) = "Length: " ++ show len
  show (IsText    text) = "Is " ++ show text
  show AnyText          = "Text"



data NumberTy = 
    IsNumber      Number
  | GreaterThan   Number
  | LessThan      Number
  | InRange       Number Number  -- | Inclusive
  -- TODO do not allow ranges [a,b] where b < a
  | Even
  | Odd
  | Integer
  | NonNegative
  | AnyNumber
  deriving (Eq, Generic)

instance Hashable NumberTy

instance Show NumberTy where
  show (IsNumber    num) = "Is " ++ show num
  show (GreaterThan lb ) = "Greater Than " ++ show lb
  show (LessThan    ub ) = "Lesser Than " ++ show ub
  show (InRange  x y   ) = 
    "[" ++ show x ++ ", " ++ show y ++ "]"
  show Even              = "Even"
  show Odd               = "Odd"
  show Integer           = "Integer"
  show NonNegative       = "NonNegative"
  show AnyNumber         = "Number"



-- Always compiled value
data SymbolTy =
    IsSymbol  Symbol
  | AnySymbol
  deriving (Eq, Generic)

instance Hashable SymbolTy

instance Show SymbolTy where
  show (IsSymbol sym) = "Is " ++ show sym
  show AnySymbol      = "Symbol"



data VariableTy = VarTy Type
  deriving (Eq, Generic)

instance Hashable VariableTy

instance Show VariableTy where
  show (VarTy varIdTy) = "Variable of " ++ show varIdTy



data DateTimeTy = 
    IsDateTime DateTime
  | AnyDateTime
  deriving (Eq, Generic)

instance Hashable DateTimeTy

instance Show DateTimeTy where
  show (IsDateTime dtm) = "Is " ++ show dtm
  show AnyDateTime      = "DateTime"


---------------------------------------------------------------------
-- System
---------------------------------------------------------------------

type Env = HMS.HashMap Val Val


data SymbolTable = SymTable
  { _tblCount ∷  Int
  , _tblMap   ∷  HMS.HashMap Int T.Text
  }


newSymbolTable ∷ SymbolTable
newSymbolTable = SymTable 0 HMS.empty





data Particle = Particle
  { _partId   ∷  Val
  , _partVal  ∷  Val
  }
  deriving (Eq, Generic)


instance Hashable Particle

instance Show Particle where
  show (Particle ident val) = 
    "Particle\n" ++ show ident ++ "\n"
                 ++ show val  ++ "\n"


data System = System 
  { _sysVal     ∷  Val
  , _sysEnv     ∷  Env
  , _symTable   ∷  SymbolTable
  }





---------------------------------------------------------------------
-- Value Lenses
---------------------------------------------------------------------

data Lens = 
    Lens_Set    SetLens
  | Lens_Arr    ArrayLens
  | Lens_This


data SetLens = 
    AllSuchThat Type Lens
  | AnySuchThat Type Lens


data ArrayLens =
    AtIndices [(Number, Lens)]
  | EachIndex Lens



getFromFst ∷ Lens → Lens
getFromFst lens = Lens_Arr $ AtIndices [(Z 0, lens)]


getFromSnd ∷ Lens → Lens
getFromSnd lens = Lens_Arr $ AtIndices [(Z 1, lens)]


getFromBoth ∷ Lens → Lens → Lens
getFromBoth lensA lensB = Lens_Arr $
    AtIndices [(Z 0, lensA), (Z 1, lensB)]


atIndex ∷ Number → Lens → Lens
atIndex idx lens = Lens_Arr $ AtIndices [(idx, lens)]


---------------------------------------------------------------------
-- Value Constructors
---------------------------------------------------------------------

{-
data RelDef = RelDef
  { _relName  ∷  Text  -- | Name of relation
  , _domain   ∷  Text  -- | Name of domain set
  , _codomain ∷  Text  -- | Name of codomain set
  }

type ParticleId = Val

type RelMap = HMS.HashMap RelDef Relation

type Relation = HMS.HashMap ParticleId [ParticleId]

type RelTuple = (ParticleId, ParticleKey)



data Query = 
  Query
    -- | Match ParticleIDs 
    Matcher
    -- | Filter matched particles by value
    Type
    -- | Extract values from matched particles
    Type



data Matcher = 
    MatchType Type
  | MatchRel  Relation


-}


data Constructor = 
    Con_Val Val
 -- | Con_Query Query
  -- TODO Generator...how/what...constraints?



---------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------

type FunDict = HMS.HashMap T.Text FunDef

data FunDef = FunDef
  { _funParamTys  ∷  [Type]
  , _funReturnTy  ∷  Type
  , _funExec      ∷  [Val] → Val
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



---------------------------------------------------------------------
-- Constructors
---------------------------------------------------------------------


---------------------------------------------------------------------
-- Lenses
---------------------------------------------------------------------

makeLenses ''System
makeLenses ''SymbolTable



