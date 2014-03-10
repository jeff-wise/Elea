

module Elea.Lang.Atom.Index.Type (
    TypeIndex
  , newTypeIndex
  , insert, lookup
  ) where


import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Atom.Val


import Control.Lens

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



-- Or and And types
--
-- Or, just insert multiple times?
--
-- abstract over keys
-- and/or create new key for each case
-- but ands...must return each key



---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------


-- | A key which identifies a particular type.
-- If a value matches a type at some type node, then
-- that type node should return the corresponding key.
type MatchKey = Int

type KeySet = Set.Set MatchKey


data InsCtx = InsCtx KeyCounter MatchKey



-- | Type Index
-- Explanation...
data TypeIndex = TypeIndex
  { _tyNode       ∷ Node_Ty
  , _tyMap        ∷ HMS.HashMap MatchKey Type
  , _tyKeyCounter ∷ Int
  }



data Node_Ty = Node_Ty
  , _node_DictTy  ∷  Node_DictTy
  , _node_ArrTy   ∷  Node_ArrTy
  , _node_SetTy   ∷  Node_SetTy
  , _node_TextTy  ∷  Node_TextTy
  , _node_NumTy   ∷  Node_NumTy
  , _node_AndTy   ∷  Node_AndTy
  }



data Node_DictTy = Node_DictTy
  { _hasEntryTy   ∷ HMS.HashMap T.Text Node_Ty
  , _dictOfSizeTy ∷ HMS.HashMap Int KeySet
  , _anyDictTy    ∷ KeySet
  }


data Node_ArrTy = Node_ArrTy
  { _withIdxTy    ∷ HMS.HashMap Int Node_Ty
  , _arrOfSizeTy  ∷ HMS.HashMap Int KeySet
  , _anyArrTy     ∷ KeySet
  }


data Node_SetTy = Node_SetTy
  { _withElemTy     ∷  Node_Ty
  , _setOfSizeTy  ∷  HMS.HashMap Int KeySet
  , _anySetTy       ∷  KeySet
  }


-- Says which combinatios of returned matched keys
-- constitute a valid match. 'Logic' to determine
-- which matches are correct at a node.
data Node_AndTy = Node_AndTy
  { _keyTotalMap  ∷  HMS.HashMap MatchKey Int
  , _keyTypeMap   ∷  HMS.HashMap MatchKey KeyType
  }

data KeyType = KeyFinal | KeyRef MatchKey



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




-- Lenses
makeLenses ''Node_Ty
makeLenses ''Node_SetTy
makeLenses ''Node_ArrTy
makeLenses ''Node_TextTy
makeLenses ''Node_NumTy
makeLenses ''Node_SymTy




---------------------------------------------------------------------
-- Constructors
---------------------------------------------------------------------


newTypeIndex ∷ TypeIndex
newTypeIndex = TypeIndex
  { _tyNode       = newTypeNode
  , _tyMap        = HMS.empty
  , _tyKeyCounter = 0
  }


-- | Lazily build a type node 
newTypeNode ∷ Node_Ty
newTypeNode = Node_Ty
  { _node_DictTy  = Node_DictTy
                      { _hasEntryTy   = HMS.empty
                      , _dictOfSizeTy = HMS.empty
                      , _anyDictTy    = Set.empty
  , _node_SetTy   = Node_SetTy
                      { _withElemTy   = newTypeNode
                      , _setOfSizeTy  = HMS.empty
                      , _anySetTy     = Set.empty
                      }  
  , _node_ArrTy   = Node_ArrTy
                      { _withIdxTy    = HMS.empty
                      , _arrOfSizeTy  = HMS.empty
                      , _anyArrTy     = Set.empty
                      }
  , _node_AndTy   = Node_AndTy
                      { _withIdxTy    = HMS.empty
                      , _arrOfSizeTy  = HMS.empty
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
  }




---------------------------------------------------------------------
-- Insert Type
---------------------------------------------------------------------



insert ∷ Type → TypeIndex → TypeIndex
insert ty (TypeIndex tyNode tyMap tyKeyCounter) =
  TypeIndex {
      _tyNode       = insertTy ty tyKeyCounter tyNode
    , _tyMap        = HMS.insert tyKeyCounter ty tyMap
    , _tyKeyCounter = tyKeyCounter + 1
  }



insertTy ∷ Type → InsCtx → Node_Ty → Node_Ty

insertTy (Ty_Dict dictTy) insCtx tyNode =
  tyNode & node_DictTy  %~ (insertDictTy dictTy insCtx)

insertTy (Ty_Arr  arrTy ) insCtx tyNode =
  tyNode & node_ArrTy   %~ (insertArrTy  arrTy  insCtx)

insertTy (Ty_Set  setTy ) insCtx tyNode =
  tyNode & node_SetTy   %~ (insertSetTy  setTy  insCtx)

insertTy (Ty_And  andTy ) insCtx tyNode =
  tyNode & node_AndTy   %~ (insertAndTy  andTy  insCtx)

insertTy (Ty_Or   orTy  ) insCtx tyNode =
  tyNode & node_OrTy    %~ (insertOrTy   orTy   insCtx)

insertTy (Ty_Text textTy) insCtx tyNode =
  tyNode & node_TextTy  %~ (insertTextTy textTy insCtx)

insertTy (Ty_Num  numTy ) insCtx tyNode =
  tyNode & node_NumTy   %~ (insertNumTy  numTy  insCtx)



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
insertSetTy AnySet               key setTyNode =
  setTyNode & anySetTy %~ (Set.insert key)



-- TODO negative indices
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
  case asInt num of
    Just idx → arrTyNode & withIdxTy %~ 
                  HMS.insertWith 
                    (\_ node → insertTy ty key node)
                    idx
                    (insertTy ty key newTypeNode)
    Nothing  → arrTyNode
insertArrTy AnyArray           key arrTyNode =
  arrTyNode & anyArrTy %~ (Set.insert key)



-- An OR type contains many types which can each be treated
-- as the type of the expression. So we insert the same key
-- for each type in the OR type list


insertOrTy ∷ AndTy → MatchKey → Node_AndTy → Node_AndTy
  (insertTy ty2 insCtx . insertTy ty1 insCtx) tyNode

insertAndTy ∷ AndTy → MatchKey → Node_AndTy → Node_AndTy
insertAndTy (AndTy ty1 ty2) key (Node_AndTy (tyNode1, tyNode2)) =
  Node_AndTy $ (insertTy ty1 key tyNode1, insertTy ty2 key tyNode2)


data Node_AndTy = Node_AndTy
  { _keyTotalMap  ∷  HMS.HashMap MatchKey Int
  , _keyTypeMap   ∷  HMS.HashMap MatchKey KeyType
  }

data KeyType = KeyFinal | KeyRef MatchKey





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




---------------------------------------------------------------------
-- Lookup
---------------------------------------------------------------------

lookup ∷ Val → TypeIndex → [Type]
lookup val (TypeIndex tyNode tyMap _) =
  let getItem key = fromJust $ HMS.lookup key tyMap
  in  L.map getItem $ Set.toList $ lookupTy val tyNode




lookupTy ∷ Val → Node_Ty → KeySet
lookupTy (Val_Set  set ) tyNode = lookupSetTy  set  (tyNode ^. node_SetTy )
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
      anySetKeys    = setTyNode ^. anySetTy
  in  withElemsKeys `Set.union` withSizeKeys `Set.union` anySetKeys




lookupArrTy ∷ Array → Node_ArrTy → KeySet
lookupArrTy (Arr arr) arrTyNode =
  let isArrKeys  =  
        let go _              EmptyL                    = Set.empty
            go EmptyL         _                         = Set.empty
            go (val :< remVals) (tyNode :< remTyNodes)
              | Seq.null remVals  = lookupTy val tyNode
              | otherwise         = Set.intersection
                                      (lookupTy val tyNode)
                                      (go (viewl remVals) (viewl remTyNodes))
        in  go (viewl arr) (viewl (arrTyNode ^. isArrTy))
      withIdxKeys = 
        let unionKeysWithIdx prevMatches nextIndex nextElem = 
              prevMatches
                `Set.union`
              (maybe 
                Set.empty 
                (lookupTy nextElem)
                (HMS.lookup nextIndex $ arrTyNode ^. withIdxTy))
        in  Seq.foldlWithIndex unionKeysWithIdx Set.empty arr
      anyArrKeys = arrTyNode ^. anyArrTy
  in  isArrKeys `Set.union` withIdxKeys `Set.union` anyArrKeys




lookupTextTy ∷ Text → Node_TextTy → KeySet
lookupTextTy text textTyNode =
  let withTextLenKeys = maybe Set.empty id $
        HMS.lookup (T.length $ _getText text) (textTyNode ^. withTextLenTy)
      isTextKeys = maybe Set.empty id $
        HMS.lookup text (textTyNode ^. isTextTy)
      anyTextKeys = textTyNode ^. anyTextTy
  in  withTextLenKeys `Set.union` isTextKeys `Set.union` anyTextKeys




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
        in  (aboveLB `Set.union` areLB) `Set.intersection` 
            (belowUB `Set.union` areUB)
      evenKeys   = if numEven   num then numTyNode ^. evenTy
                                    else Set.empty 
      oddKeys    = if numOdd    num then numTyNode ^. oddTy
                                    else Set.empty 
      intKeys    = if isInteger num then numTyNode ^. intTy
                                    else Set.empty 
      nonNegKeys = if num >= (Z 0)  then numTyNode ^. nonNegTy 
                                    else Set.empty 
      anyNumKeys = numTyNode ^. anyNumTy
  in  L.foldl' Set.union Set.empty [ isNumKeys, gtKeys, ltKeys,
                                     rangeKeys, evenKeys, oddKeys,
                                     intKeys, nonNegKeys, anyNumKeys ]
  

