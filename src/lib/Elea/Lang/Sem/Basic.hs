

-- | Operational semantics for basic terms
module Elea.Lang.Sem.Basic
  ( -- * Type Semantics 
    isType
  , isDictType, isArrType, isSetType
  , isAndType, isOrType
  , isTextType, isNumType, isDtmType
  ) where




---------------------------------------------------------------------
-- Type Semantics
---------------------------------------------------------------------

isType ∷ Type → Val → Bool

isType (Ty_Dict dictTy) (Val_Dict dict) = isDictType dictTy dict

isType (Ty_Arr  arrTy ) (Val_Arr  arr ) = isArrType  arrTy  arr

isType (Ty_Set  setTy ) (Val_Set  set ) = isSetType  setTy  set

isType (Ty_And  andTy ) val             = isAndType  andTy  val

isType (Ty_Or   orTy  ) val             = isOrType   orTy   val

isType (Ty_Text textTy) (Val_Text text) = isTextType textTy text

isType (Ty_Num  numTy ) (Val_Num  num ) = isNumType  numTy  num

isType (Ty_Dtm  dtmTy ) (Val_Dtm  dtm ) = isDtmType  dtmTy  dtm

isType  Ty_Any          _               = True

isType _                _               = False




isDictType ∷ DictTy → Dict → Bool

isDictType (HasEntry key ty  ) (Dict hm) =
  case HMS.lookup key hm of
    Just val  → isType ty val 
    Nothing   → False

isDictType (IsDict entryTypes) (Dict hm) =
  let checkDict []               hm = null hm
      checkDict ((key, ty):rest) hm =
        case HMS.lookup key hm of
          Just val  → (isType ty val) &&
                       (checkDict rest $ delete key hm)
          Nothing   → False
  in  checkDict entryTypes hm




isSetType ∷ SetTy → Set → Bool
isSetType (WithElem    ty  ) (Set set)  = L.or $
                                            L.map (isType ty) (HS.toList set)

isSetType (SetWithSize size) (Set set)  = HS.size set == size

isSetType AnySet             _          = True




isAndType ∷ AndTy → Val → Bool
isAndType (AndTy typeList) val =
  and $ map (flip $ isType val) typeList




isOrType ∷ OrTy → Val → Bool
isOrType (OrTy typeList) val =
  or $ map (flip $ isType val) typeList




isArrType ∷ ArrayTy → Array → Bool

isArrType (IsArray   elemTypes) (Arr arr) =
  let checkArr EmptyL         EmptyL             = True
      checkArr EmptyL         _                  = False
      checkArr _              EmptyL             = False
      checkArr (ty :< remTys) (elem :< remElems) =
        isType ty elem &&
        (checkArr (Seq.viewl remTys) (Seq.viewl remElemes))
  in  checkArr (Seq.viewl elemTypes) (Seq.viewl arr)

isArrType (WithIndex idx ty) array = 
  maybe False (isType ty) $ array `at` idx

isArrType AnyArray          _     = True




isTextType ∷ TextTy → Text → Bool

isTextType (WithTextLen len   ) (Text text) = T.length text == len

isTextType (IsText      isText) (Text text) = isText == text

isTextType AnyText              _           = True




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




isDtmType ∷ DateTimeTy → DateTime → Bool

isDtmType (IsDateTime isDtm) dtm = isDtm == dtm

isDtmType AnyDateTime        _   = True 


