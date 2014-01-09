


module Elea.Lang.Type where


import Elea.Prelude
import Elea.Lang.Types
import Elea.Lang.Val (at, doubleToInt, isInteger)

import qualified Data.Foldable as F
import qualified Data.HashSet as Set
import qualified Data.List.Stream as L
import qualified Data.Sequence as Seq
import qualified Data.Text as T





{-
-- Convert a value x to a type s
-- such that s is a subset of any type
-- t which is a also a type of x

typeOf ∷ Val → Type
typeOf (Val_Set  set )  = Set_EQ $ map typeOf $ elems set
typeOf (Val_Pair a b )  = Pair_EQ $ typeOf a $ typeOf b
typeOf (Val_Arr  seq )  = Arr_EQ $ typeOf <$> seq
typeOf (Val_Text text)  = Text_EQ text
typeOf (Val_Num  num )  = Num_EQ num
typeOf (Val_Sym  sym )  = Sym_EQ sym
typeOf (Dtm  dtm )  = Dtm_EQ dtm
-}



isType ∷ Type → Val → Bool
isType (Ty_Set  setTy ) (Val_Set  set ) = isSetType  setTy  set
--isType (Ty_Pair pairTy) (Val_Pair pair) = isPairType pairTy pair
isType (Ty_Arr  arrTy ) (Val_Arr  arr ) = isArrType  arrTy  arr
isType (Ty_And  andTy ) val             = isAndType  andTy  val
isType (Ty_Or   orTy  ) val             = isOrType   orTy   val
isType (Ty_Text textTy) (Val_Text text) = isTextType textTy text
isType (Ty_Num  numTy ) (Val_Num  num ) = isNumType  numTy  num
isType (Ty_Sym  symTy ) (Val_Sym  sym ) = isSymType  symTy  sym
isType (Ty_Dtm  dtmTy ) (Val_Dtm  dtm ) = isDtmType  dtmTy  dtm
isType  Ty_Any          _               = True
isType _                _               = False


--isPairType ∷ PairTy → Pair → Bool
--isPairType (IsPair tyA tyB) (Pair a b) = 
--  isType tyA a && isType tyB b
--isPairType (First  ty      ) (Pair a _) = isType ty a
--isPairType (Second ty      ) (Pair _ b) = isType ty b
--isPairType AnyPair           _          = True


isSetType ∷ SetTy → Set → Bool
isSetType (WithElem    ty      ) (Set set) = 
  L.or $ L.map (isType ty) (Set.toList set)
isSetType (SetWithSize (Z size)) (Set set) = Set.size set == size
-- isSetType (IsSet       elemTys ) (Set set) =
isSetType AnySet                  _         = True
isSetType _                       _         = False


isAndType ∷ AndTy → Val → Bool
isAndType (AndTy ty1 ty2) val = isType ty1 val && isType ty2 val


isOrType ∷ OrTy → Val → Bool
isOrType (OrTy ty1 ty2 ) val = isType ty1 val || isType ty2 val


isArrType ∷ ArrayTy → Array → Bool
isArrType (IsArray   elemTys ) (Arr arr) =
  if (Seq.length elemTys) == (Seq.length arr)
    then  F.foldr (&&) True $
            (flip fmap) (elemTys `Seq.zip` arr) $
              (\(ty, elem) → isType ty elem)
    else False
isArrType (WithIndex (Z i) ty) array = 
  maybe False (isType ty) $ array `at` i
isArrType AnyArray          _     = True
isArrType _                 _     = False


isTextType ∷ TextTy → Text → Bool
isTextType (WithTextLen (Z len)      ) (Text text) = T.length text == len
isTextType (IsText    (Text isText)) (Text text) = isText == text
isTextType AnyText                   _           = True
isTextType _                   _           = False



isNumType ∷ NumberTy → Number → Bool
isNumType (IsNumber     isNum     ) num   = isNum == num
isNumType (GreaterThan  lowerBound) num   = num > lowerBound
isNumType (LessThan     upperBound) num   = num < upperBound
isNumType (InRange      i   j     ) num   = num >= i && num <= j
isNumType Even                      (Z i) = even i
isNumType Even                      (R d) = maybe False even $ doubleToInt d
isNumType Odd                       (Z i) = odd i
isNumType Odd                       (R d) = maybe False odd $ doubleToInt d
isNumType Integer                   num   = isInteger num
isNumType NonNegative               (Z i) = i >= 0 
isNumType NonNegative               (R r) = r >= 0 
isNumType AnyNumber                 _     = True


isSymType ∷ SymbolTy → Symbol → Bool
isSymType (IsSymbol isSymbol) symbol = isSymbol == symbol


isDtmType ∷ DateTimeTy → DateTime → Bool
isDtmType (IsDateTime isDtm) dtm = isDtm == dtm
isDtmType AnyDateTime        _   = True 


