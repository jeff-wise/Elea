

module Elea.Lang.Term.TypeCons where



-- only type level function
-- on purpose. type level computations
--   handled by advanced refinement types
-- just need to create refinement types from values

data Var a = 
    Param T.Text
  | Decl a



data TypeCons = 
    Cons_Ty_Dict   Cons_DictTy
  | Cons_Ty_Set    Cons_SetTy
  | Cons_Ty_Arr    Cons_ArrayTy
  | Cons_Ty_And    Cons_AndTy
  | Cons_Ty_Or     Cons_OrTy
  | Cons_Ty_Text   Cons_TextTy
  | Cons_Ty_Num    Cons_NumberTy
  | Cons_Ty_Dtm    Cons_DateTimeTy
  | Cons_Ty_Any 


data Cons_DictTy =
    Cons_HasEntry   (Var T.Text) (Var TypeCons)
  | Cons_DictOfSize (Var Int)
  | Cons_AnyDict


data T_SetTy = 
    T_WithElem      (Var TypeCons)
  | T_SetWithSize   (Var Int)
  | T_AnySet


data T_ArrayTy = 
    T_WithIndex   (Var Int) (Var TypeCons)
  | T_ArrWithSize (Var Int)
  | T_AnyArray


data T_AndTy = T_AndTy [(Var TypeCons)]


data T_OrTy = T_OrTy [(Var TypeCons)]


data T_TextTy = 
    T_WithTextLen  Int
  | T_IsText       T.Text
  | T_OneOfText    (Template (HS.HashSet T.Text))
  | T_AnyText


data T_NumberTy = 
    T_IsNumber      (Var Number)
  | T_GreaterThan   (Var Number)
  | T_LessThan      (Var Number)
  | T_InRange       (Var Number) (Var Number)
  | T_Even
  | T_Odd
  | T_Integer
  | T_NonNegative
  | T_AnyNumber


data T_DateTimeTy = 
    T_IsDateTime (Var DateTime)
  | T_AnyDateTime



