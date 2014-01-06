

module Elea.Lang.Index.Type where


import Elea.Prelude
import Elea.Lang.Types
import Elea.Lang.Val


import Control.Lens (_1, _2)

import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.List.Stream as L
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T



-- TODO unique insertion
-- TODO deletion


---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------

-- | Index Match Keys
-- Used to uniquely identify parts of types
-- in branches of the index.
type MatchKey = Int
type KeySet = Set.Set MatchKey



-- | Type Index
-- Explanation...
data TypeIndex a = TypeIndex
  { _tyNode     ∷ Node_Ty
  , _itemMap    ∷ HMS.HashMap MatchKey a
  , _keyCounter ∷ Int
  }



data Node_Ty = Node_Ty
  { _node_SetTy   ∷  Node_SetTy
  , _node_PairTy  ∷  Node_PairTy
  , _node_ArrTy   ∷  Node_ArrTy
  , _node_AndTy   ∷  Node_AndTy
  , _node_TextTy  ∷  Node_TextTy
  , _node_NumTy   ∷  Node_NumTy
  , _node_SymTy   ∷  Node_SymTy
  }



data Node_SetTy = Node_SetTy
  { _withElemTy     ∷  Node_Ty
  , _setWithSizeTy  ∷  HMS.HashMap Int KeySet
  , _isSetTy        ∷  (HMS.HashMap MatchKey Int, Node_Ty)
  , _anySetTy       ∷  KeySet
  }



data Node_PairTy = Node_PairTy
  { _isPairTy ∷ (Node_Ty, Node_Ty)
  , _firstTy  ∷ Node_Ty
  , _secondTy ∷ Node_Ty
  }



data Node_ArrTy = Node_ArrTy
  { _isArrTy    ∷ Seq.Seq Node_Ty 
  , _withIdxTy  ∷ HMS.HashMap Int Node_Ty
  , _anyArrTy   ∷ KeySet
  }



data Node_AndTy = Node_AndTy (Node_Ty, Node_Ty)



data Node_TextTy = Node_TextTy
  { _withTextLenTy ∷ HMS.HashMap Int KeySet
  , _isTextTy      ∷ HMS.HashMap Text KeySet
  , _anyTextTy     ∷ KeySet
  }



data Node_NumTy = Node_NumTy
  { _isNumTy  ∷  Map.Map Number KeySet
  , _gtNumTy  ∷  Map.Map Number KeySet
  , _ltNumTy  ∷  Map.Map Number KeySet
  , _rangeTy  ∷  ( Map.Map Number KeySet
                  , Map.Map Number KeySet)
  , _evenTy   ∷  KeySet
  , _oddTy    ∷  KeySet
  , _intTy    ∷  KeySet
  , _nonNegTy ∷  KeySet
  , _anyNumTy ∷  KeySet
  }



data Node_SymTy = Node_SymTy
  { _isSymTy  ∷  HMS.HashMap Symbol KeySet }



-- | Lenses
makeLenses ''Node_Ty
makeLenses ''Node_SetTy
makeLenses ''Node_PairTy
makeLenses ''Node_ArrTy
makeLenses ''Node_TextTy
makeLenses ''Node_NumTy




---------------------------------------------------------------------
-- Create a Type Node
---------------------------------------------------------------------

-- | Lazily build a type node 
newTypeNode ∷ Node_Ty
newTypeNode = Node_Ty
  { _node_SetTy   = Node_SetTy
                      { _withElemTy    = newTypeNode
                      , _setWithSizeTy = HMS.empty
                      , _isSetTy       = (HMS.empty, newTypeNode)
                      , _anySetTy      = Set.empty
                      }  
  , _node_PairTy  = Node_PairTy
                      { _isPairTy = (newTypeNode, newTypeNode)
                      , _firstTy  = newTypeNode
                      , _secondTy = newTypeNode
                      }
  , _node_ArrTy   = Node_ArrTy
                      { _isArrTy   = Seq.empty 
                      , _withIdxTy = HMS.empty
                      , _anyArrTy  = Set.empty
                      }
  , _node_AndTy   = Node_AndTy (newTypeNode, newTypeNode)  
  , _node_TextTy  = Node_TextTy
                      { _withTextLenTy = HMS.empty
                      , _isTextTy      = HMS.empty
                      , _anyTextTy     = Set.empty
                      }
  , _node_NumTy   = Node_NumTy
                      { _isNumTy  = Map.empty  
                      , _gtNumTy  = Map.empty
                      , _ltNumTy  = Map.empty
                      , _rangeTy  = (Map.empty, Map.empty)
                      , _evenTy   = Set.empty
                      , _oddTy    = Set.empty
                      , _intTy    = Set.empty
                      , _nonNegTy = Set.empty
                      , _anyNumTy = Set.empty
                      }
  , _node_SymTy   = Node_SymTy HMS.empty
  }




---------------------------------------------------------------------
-- Insert Type
---------------------------------------------------------------------



insert ∷ Type → a → TypeIndex a → TypeIndex a
insert ty item (TypeIndex tyNode itemMap keyCounter) =
  TypeIndex {
      _tyNode     = insertTy ty keyCounter tyNode
    , _itemMap    = HMS.insert keyCounter item itemMap
    , _keyCounter = keyCounter + 1
  }



insertTy ∷ Type → MatchKey → Node_Ty → Node_Ty
insertTy (Ty_Set  setTy         ) key tyNode =
  tyNode & node_SetTy  %~ (insertSetTy   setTy  key)
insertTy (Ty_Pair pairTy        ) key tyNode =
  tyNode & node_PairTy %~ (insertPairTy  pairTy key)
insertTy (Ty_Arr  arrTy         ) key tyNode =
  tyNode & node_ArrTy  %~ (insertArrTy   arrTy  key)
insertTy (Ty_And  andTy         ) key tyNode =
  tyNode & node_AndTy  %~ (insertAndTy   andTy  key)
insertTy (Ty_Or   (OrTy ty1 ty2)) key tyNode =
  (insertTy ty2 key . insertTy ty1 key) tyNode
insertTy (Ty_Text textTy        ) key tyNode =
  tyNode & node_TextTy  %~ (insertTextTy textTy key)
insertTy (Ty_Num  numTy         ) key tyNode =
  tyNode & node_NumTy   %~ (insertNumTy  numTy  key)
insertTy (Ty_Sym  symTy         ) key tyNode =
  tyNode & node_SymTy   %~ (insertSymTy  symTy  key)



insertSetTy ∷ SetTy → MatchKey → Node_SetTy → Node_SetTy
insertSetTy (WithElem    ty    ) key setTyNode =
  setTyNode & withElemTy %~ insertTy ty key
--insertSetTy (WithoutElem ty    ) key setTyNode =
--  setTyNode & withoutElemTy %~ 
--    HMS.insertWith Set.union ty (Set.singleton key)
insertSetTy (SetWithSize num   ) key setTyNode =
  let size = case num of
              (Z i) → i
              (R d) → maybe (-1) id $ doubleToInt d
  in  if size < 0
        then setTyNode
        else setTyNode & setWithSizeTy %~
              HMS.insertWith Set.union size (Set.singleton key)
insertSetTy (IsSet       tyList) key setTyNode =
  let update (setLenMap, tyNode) =
        let addTy node ty = insertTy ty key node
        in  ( HMS.insert key (L.length tyList) setLenMap
            , L.foldl' addTy tyNode tyList )
  in  setTyNode & isSetTy %~ update
insertSetTy AnySet               key setTyNode =
  setTyNode & anySetTy %~ (Set.insert key)



insertPairTy ∷ PairTy → MatchKey → Node_PairTy → Node_PairTy
insertPairTy (IsPair ty1 ty2) key pairTyNode =
  pairTyNode & isPairTy %~ 
    (\(tyNode1, tyNode2) → ( insertTy ty1 key tyNode1
                            , insertTy ty2 key tyNode2 )
    )
insertPairTy (First  ty     ) key pairTyNode =
  pairTyNode & firstTy %~ insertTy ty key
insertPairTy (Second ty     ) key pairTyNode =
  pairTyNode & secondTy %~ insertTy ty key



insertArrTy ∷ ArrayTy → MatchKey → Node_ArrTy → Node_ArrTy
insertArrTy (IsArray   tySeq ) key arrTyNode = 
  let go EmptyL          EmptyL                 = Seq.empty
      go (ty :< remTys)  EmptyL                 =
        fmap (\x → insertTy x key newTypeNode) (ty <| remTys)
      go EmptyL          (tyNode :< remTyNodes) = tyNode <| remTyNodes
      go (ty :< remTys)  (tyNode :< remTyNodes) =
           insertTy ty key tyNode 
        <| go (viewl remTys) (viewl remTyNodes)
  in  arrTyNode & isArrTy %~ (go (viewl tySeq) . viewl)
insertArrTy (WithIndex num ty) key arrTyNode =
  let mIdx = case num of
              (Z i) → Just i
              (R d) → doubleToInt d
  in  case mIdx of
        Just idx → arrTyNode & withIdxTy %~ 
                      (HMS.adjust (insertTy ty key) idx)
        Nothing  → arrTyNode
insertArrTy AnyArray           key arrTyNode =
  arrTyNode & anyArrTy %~ (Set.insert key)



insertAndTy ∷ AndTy → MatchKey → Node_AndTy → Node_AndTy
insertAndTy (AndTy ty1 ty2) key (Node_AndTy (tyNode1, tyNode2)) =
  Node_AndTy $ (insertTy ty1 key tyNode1, insertTy ty2 key tyNode2)



insertTextTy ∷ TextTy → MatchKey → Node_TextTy → Node_TextTy
insertTextTy (WithTextLen num ) key textTyNode =
  let len = case num of
              (Z i) → i
              (R d) → maybe (-1) id $ doubleToInt d
  in  if len < 0
        then textTyNode
        else textTyNode & withTextLenTy %~ 
              HMS.insertWith Set.union len (Set.singleton key)
insertTextTy (IsText      text) key textTyNode =
  textTyNode & isTextTy %~ 
    HMS.insertWith Set.union text (Set.singleton key)
insertTextTy AnyText            key textTyNode =
  textTyNode & anyTextTy %~ (Set.insert key)



insertNumTy ∷ NumberTy → MatchKey → Node_NumTy → Node_NumTy
insertNumTy (IsNumber    num) key numTyNode =
  numTyNode & isNumTy %~
    Map.insertWith Set.union num (Set.singleton key)
insertNumTy (GreaterThan num) key numTyNode =
  numTyNode & gtNumTy %~
    Map.insertWith Set.union num (Set.singleton key)
insertNumTy (LessThan    num) key numTyNode =
  numTyNode & ltNumTy %~
    Map.insertWith Set.union num (Set.singleton key)
insertNumTy (InRange lb ub  ) key numTyNode =
  let update (lbMap, ubMap) =
        ( Map.insertWith Set.union lb (Set.singleton key) lbMap
        , Map.insertWith Set.union ub (Set.singleton key) ubMap )
  in  numTyNode & rangeTy %~ update
insertNumTy Even              key numTyNode =
  numTyNode & evenTy %~ (Set.insert key)
insertNumTy Odd               key numTyNode =
  numTyNode & oddTy %~ (Set.insert key)
insertNumTy Integer           key numTyNode =
  numTyNode & intTy %~ (Set.insert key)
insertNumTy NonNegative       key numTyNode =
  numTyNode & nonNegTy %~ (Set.insert key)
insertNumTy AnyNumber         key numTyNode =
  numTyNode & anyNumTy %~ (Set.insert key)



insertSymTy ∷ SymbolTy → MatchKey → Node_SymTy → Node_SymTy
insertSymTy (IsSymbol sym) key (Node_SymTy symMap) = Node_SymTy $
  HMS.insertWith Set.union sym (Set.singleton key) symMap




---------------------------------------------------------------------
-- Lookup
---------------------------------------------------------------------

lookup ∷ Val → TypeIndex a → [a]
lookup val (TypeIndex tyNode itemMap _) =
  let getItem key = fromJust $ HMS.lookup key itemMap
  in  L.map getItem $ Set.toList $ lookupTy val tyNode



lookupTy ∷ Val → Node_Ty → KeySet
lookupTy (Val_Set  set ) tyNode = lookupSetTy  set  (tyNode ^. node_SetTy )
lookupTy (Val_Pair pair) tyNode = lookupPairTy pair (tyNode ^. node_PairTy)
lookupTy (Val_Arr  arr ) tyNode = lookupArrTy  arr  (tyNode ^. node_ArrTy )
lookupTy (Val_Text text) tyNode = lookupTextTy text (tyNode ^. node_TextTy)
lookupTy (Val_Num  num ) tyNode = lookupNumTy  num  (tyNode ^. node_NumTy )
lookupTy (Val_Sym  sym ) tyNode = lookupSymTy  sym  (tyNode ^. node_SymTy )



lookupSetTy ∷ Set → Node_SetTy → KeySet
lookupSetTy (Set set) setTyNode =
  let withElemsKeys = 
        let lookupElemTy = flip lookupTy $ setTyNode ^. withElemTy
            matches      = Set.fromList $ L.map lookupElemTy $ HS.toList set
        in  Set.foldl' Set.union Set.empty matches
      withSizeKeys  = maybe Set.empty id $
        HMS.lookup (HS.size set) (setTyNode ^. setWithSizeTy)
  in  withElemsKeys `Set.union` withSizeKeys



lookupPairTy ∷ Pair → Node_PairTy → KeySet
lookupPairTy (Pair a b) pairTyNode =
  let isPairKeys   =  
        let (tyNodeA, tyNodeB) = pairTyNode ^. isPairTy
        in  lookupTy a tyNodeA `Set.intersection`
            lookupTy b tyNodeB
      isFirstKeys  = lookupTy a $ pairTyNode ^. isPairTy._1
      isSecondKeys = lookupTy b $ pairTyNode ^. isPairTy._2
  in  isPairKeys `Set.union` isFirstKeys `Set.union` isSecondKeys



lookupArrTy ∷ Array → Node_ArrTy → KeySet
lookupArrTy arrVal@(Arr arr) arrTyNode =
  let isArrKeys  =  
        let go EmptyL           EmptyL                 keys = keys
            go _                EmptyL                 _    = Set.empty
            go EmptyL           _                      keys = keys
            go (val :< remVals) (tyNode :< remTyNodes) keys = 
              go (viewl remVals) (viewl remTyNodes) $
                Set.intersection keys (lookupTy val tyNode)
        in  go (viewl arr) (viewl (arrTyNode ^. isArrTy)) Set.empty
      withIdxKeys = 
        let unionKeysWithIdx prevMatches idx tyNode = 
              let matchesAtCurrIdx = case arrVal `at` idx of
                                       Just val → lookupTy val tyNode
                                       Nothing  → Set.empty
              in  Set.union prevMatches matchesAtCurrIdx
        in  HMS.foldlWithKey' unionKeysWithIdx Set.empty
              (arrTyNode ^. withIdxTy)
      anyArrKeys = arrTyNode ^. anyArrTy
  in  isArrKeys `Set.union` withIdxKeys `Set.union` anyArrKeys



lookupAndTy ∷ Val → Node_AndTy → KeySet
lookupAndTy val (Node_AndTy (tyNode1, tyNode2)) = 
  lookupTy val tyNode1 `Set.intersection` lookupTy val tyNode2



lookupTextTy ∷ Text → Node_TextTy → KeySet
lookupTextTy text textTyNode =
  let withTextLenKeys = maybe Set.empty id $
        HMS.lookup (T.length $ _getText text) (textTyNode ^. withTextLenTy)
      isTextKeys = maybe Set.empty id $
        HMS.lookup text (textTyNode ^. isTextTy)
      anyTextKeys = textTyNode ^. anyTextTy
  in  withTextLenKeys `Set.union` isTextKeys `Set.union` anyTextKeys


lookupSymTy ∷ Symbol → Node_SymTy → KeySet
lookupSymTy symbol (Node_SymTy symMap) =
  maybe Set.empty id $ HMS.lookup symbol symMap



lookupNumTy ∷ Number → Node_NumTy → KeySet
lookupNumTy num numTyNode =
  let isNumKeys  =  maybe Set.empty id $ 
                      Map.lookup num (numTyNode ^. isNumTy)
      gtKeys     =  Map.foldl' Set.union Set.empty $
                      fst $ Map.split num (numTyNode ^. gtNumTy)
      ltKeys     =  Map.foldl' Set.union Set.empty $
                      snd $ Map.split num (numTyNode ^. ltNumTy)
      rangeKeys  =
        let (lbMap, ubMap) = numTyNode ^. rangeTy
            aboveLB = Map.foldl' Set.union Set.empty $
                        fst $ Map.split num lbMap
            areLB   = maybe Set.empty id $ Map.lookup num lbMap
            belowUB = Map.foldl' Set.union Set.empty $
                        snd $ Map.split num ubMap
            areUB   = maybe Set.empty id $ Map.lookup num ubMap
        in  aboveLB `Set.union` areLB `Set.union` 
            belowUB `Set.union` areUB
      evenKeys   = if numEven   num then numTyNode ^. evenTy
                                    else Set.empty 
      oddKeys    = if numOdd    num then numTyNode ^. oddTy
                                    else Set.empty 
      intKeys    = if isInteger num then numTyNode ^. intTy
                                    else Set.empty 
      nonNegKeys = if num > (Z 0)   then numTyNode ^. nonNegTy 
                                    else Set.empty 
      anyNumKeys = numTyNode ^. anyNumTy
  in  L.foldl' Set.union Set.empty [ isNumKeys, gtKeys, ltKeys,
                                     rangeKeys, evenKeys, oddKeys,
                                     intKeys, nonNegKeys, anyNumKeys ]
  

