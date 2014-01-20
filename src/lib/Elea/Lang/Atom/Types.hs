

module Elea.Lang.Atom.Types
  ( -- * Values
    Val (..)
  , Set (..), Array (..), pair
  , Text (..), Number (..), Symbol (..)
  , DateTime (..), Variable (..)
  , Error (..)
  , SynError (..), AppError (..), ParamError (..)
  -- * Types
  , Type (..)
  , SetTy (..), ArrayTy (..)
  , pairTy, fstTy, sndTy
  , AndTy (..), OrTy (..)
  , TextTy (..), NumberTy (..), SymbolTy (..)
  , DateTimeTy (..), VariableTy (..)
  ) where
 

import Elea.Prelude


import qualified Data.HashSet as HS
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
  | Val_Rel     Relation
  | Val_Dtm     DateTime
  | Val_Var     Variable
  | Val_Err     Error
  | Val_Null
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
  show (Val_Null     ) = "Null"



data Set = Set
  { _getSet ∷ HS.HashSet Val } 
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


data Symbol = Symbol Int
  deriving (Eq, Generic)

instance Hashable Symbol

instance Show Symbol where
  show (Symbol symId) = "Symbol " ++ show symId




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

data Error =
    Err_Syn Text SynError
  | Err_Proc ProcessError
  deriving (Eq, Generic)

instance Hashable Error

instance Show Error where
  show (Err_Syn synName err) =
    "Error in Synthesis " ++ show synName
    ++ "\nError:\n" ++ show err



data ProcessError =
    CannotFindSystem Val
  | ProcessAlreadyExists Val
  deriving (Eq, Generic)

instance Hashable ProcessError


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
  | IsSet         (HS.HashSet Type)
  | SetWithSize   Number
  | AnySet
  deriving (Eq, Generic)


-- how to implement isSet
-- check each one, get matches
-- make sure params are not subtypes of each other?

instance Hashable SetTy

instance Show SetTy where
  show (WithElem ty) = 
    "WithElem of " ++ show ty
  show (SetWithSize size) = 
    "Set with size of " ++ show size
  show (IsSet set) = "Is " ++ show set
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



data VariableTy = IsVariable Type
  deriving (Eq, Generic)

instance Hashable VariableTy

instance Show VariableTy where
  show (IsVariable varIdTy) = "Variable of " ++ show varIdTy



data DateTimeTy = 
    IsDateTime DateTime
  | AnyDateTime
  deriving (Eq, Generic)

instance Hashable DateTimeTy

instance Show DateTimeTy where
  show (IsDateTime dtm) = "Is " ++ show dtm
  show AnyDateTime      = "DateTime"



