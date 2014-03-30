

module Elea.Lang.Index.Type (
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



---------------------------- TYPES -------------------------


-- | A key which identifies a particular type.
-- If a value matches a type at some type node, then
-- that type node should return the corresponding key.
type MatchKey = Int


-- | A set of keys
-- A keyset at a terminating node indicates that
-- its path intersects multiple types
type KeySet = Set.Set MatchKey


-- | Type of 'MatchKey'
data KeyType =
    -- | The key represents an inserted type
    KeyFinal        
    -- | The key is part of an intersection type
  | KeyRef MatchKey

-- | Ensure that all keys are unique
type KeyCounter = Int



-- | Type Index
-- Explanation...
data TypeIndex = TypeIndex
  { _tyNode       ∷ Node_Ty
  , _tyMap        ∷ HMS.HashMap MatchKey Type
  , _tyKeyCounter ∷ Int
  }


-- | Type Node
-- Merges multiple types into one large union type.
--
--          *           *                    *
--          / \         / \              /       \
--          G  Y       H   Z   ==>     (G1,H2)   (Y1,Z2)
--         / \  \     /               /      \      \
--        A   B  C   L              (A1,L2)  (B1)    (C1)
--
--  * Enables types with similar structure to be examined
--    together, without traversing the same paths
--    in each type repeatedly.
--    Instead of searching G -> A, and then H -> L, a lookup
--    now only needs to examine (G1,H2) -> (A1,L2), reducing
--    the number of node traversals which could be expensive
--    when many similar types are being examined.
--  * Types that have same structure but vary slightly may
--    be merged into an optimal structure e.g. use of ordered
--    maps with numeric types.
--    Suppose (G1,H2) were each IsNumber types. Instead of
--    individually checking our input against each IsNumber type
--    at the node, we could store G1, H2, etc.. in an ordered
--    map (binary tree) for fast lookup of all types which are
--    equal to the input at this node.
--
--  Intersection types have multiple member types which must
--  each be satisfied in order to satisfy the type.
--  Therefore, all inserted keys are marked as final or hypo.
--  Final keys are returned. Hypo keys are added to a set of
--  hypotheses for an associated key. That associated key is
--  'derived' when all of its antecedent keys are discovered.
--  The derived key may be final or hypothetical.
--
data Node_Ty = Node_Ty
  { _node_RecTy   ∷  Node_RecTy
  , _node_ArrTy   ∷  Node_ArrTy
  , _node_SetTy   ∷  Node_SetTy
  , _node_TextTy  ∷  Node_TextTy
  , _node_NumTy   ∷  Node_NumTy
  , _keyMap       ∷  HMS.HashMap MatchKey KeyType
  }



data Node_RecTy = Node_RecTy
  { _hasEntryTy   ∷ HMS.HashMap T.Text Node_Ty
  , _recOfSizeTy ∷ HMS.HashMap Int KeySet
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
makeLenses ''Node_RecTy
makeLenses ''Node_ArrTy
makeLenses ''Node_SetTy
makeLenses ''Node_TextTy
makeLenses ''Node_NumTy




------------------------ CONSTRUCTORS ----------------------

-- | Create a new Type Index
newTypeIndex ∷ TypeIndex
newTypeIndex = TypeIndex
  { _tyNode       = newTypeNode
  , _tyMap        = HMS.empty
  , _tyKeyCounter = 0
  }


-- | Lazily construct a new Type Node
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




--------------------------- INSERT -------------------------


insert ∷ Type → TypeIndex → TypeIndex
insert ty (TypeIndex tyNode tyMap tyKeyCounter) =
  TypeIndex {
      _tyNode       = insertTy ty tyKeyCounter tyNode
    , _tyMap        = HMS.insert tyKeyCounter ty tyMap
    , _tyKeyCounter = tyKeyCounter + 1
  }



insertTy ∷ Type → MatchKey → Node_Ty → Node_Ty

insertTy (Ty_Dict dictTy) key tyNode =
  tyNode & node_DictTy  %~ (insertDictTy dictTy key)

insertTy (Ty_Arr  arrTy ) key tyNode =
  tyNode & node_ArrTy   %~ (insertArrTy  arrTy  key)

insertTy (Ty_Set  setTy ) key tyNode =
  tyNode & node_SetTy   %~ (insertSetTy  setTy  key)

insertTy (Ty_And  andTy ) key tyNode =
  tyNode & node_AndTy   %~ (insertAndTy  andTy  key)

insertTy (Ty_Or   orTy  ) key tyNode =
  tyNode & node_OrTy    %~ (insertOrTy   orTy   key)

insertTy (Ty_Text textTy) key tyNode =
  tyNode & node_TextTy  %~ (insertTextTy textTy key)

insertTy (Ty_Num  numTy ) key tyNode =
  tyNode & node_NumTy   %~ (insertNumTy  numTy  key)




insertSetTy ∷ SetTy → InsContext → Node_SetTy → Node_SetTy

insertSetTy (WithElem    ty  ) key setTyNode =
  setTyNode & withElemTy %~ insertTy ty key

insertSetTy (SetOfSize size) key setTyNode =
  let updateSizeNode = HMS.insertWith Set.union size (Set.singleton key)
  in  if size < 0 then setTyNode
                  else setTyNode & setWithSizeTy %~ updateSizeNode

insertSetTy AnySet               key setTyNode =
  setTyNode & anySetTy %~ (Set.insert key)




insertArrTy ∷ ArrayTy → MatchKey → Node_Ty → Node_ArrTy
insertArrTy (WithIndex num ty) key arrTyNode =
  case asInt num of
    Just idx → arrTyNode & withIdxTy %~ 
                  HMS.insertWith 
                    (\_ node → insertTy ty key node)
                    idx
                    (insertTy ty key newTypeNode)
    Nothing  → arrTyNode

insertArrTy (ArrOfSize size  ) key arrTyNode =
  let updateSizeNode = HMS.insertWith Set.union size (Set.singleton key)
  in  if size < 0 then arrTyNode
                  else arrTyNode & arrOfSizeTy %~ updateSizeNode

insertArrTy AnyArray           key arrTyNode =
  arrTyNode & anyArrTy %~ (Set.insert key)



-- | An Or type is an untagged union of types, so it may
-- be indexed simply by inserting each type of the union
-- into the node using the same key.
-- When a lookup returns that key, then THERE EXISTS some type
-- in the union type that matches the given value.
insertOrTy ∷ OrTy → Matchkey → Node_Ty → NodeUpdate
insertOrTy (OrTy tyList) key tyNode =
  let insOrTy node ty = insertTy ty key node
  in  L.foldl' insOrTy tyNode tyList




-- | An And type is an intersection type. Unlike Or types, we
-- must track each member type separately, to ensure that a
-- given lookup value matches FOR ALL types in the And type set.
-- Creates a NEW key for each type and inserts that key into the
-- node. Does so repeatedly until a non-And type is found. Marks
-- the initial key as final.
insertAndTy ∷ AndTy → MatchKey → Node_Ty → NodeUpdate
insertAndTy (AndTy tyList) key tyNode =

  let -- Adds key of some type as child of the AND Type
      updateKeyMap newKey node = 
          node & _node_AndTy %~
            (over andKeyMap (HMS.insert newKey $ KeyRef key))
      
      --  Get key for type, add to node, and record key
      addAndTy node ty = do
        newKey ← get 
        modify (+1)
        return $ updateKeyMap newKey node 
        insertTy ty newKey node
    
      -- Mark input key as final state
      -- Record size of AND Type
      updateKeyTotal =  
  
  in  F.foldrM addAndTy tyNode tyList
    >>= (

  return $ tyNode & _node_AndTy ~ andNode'
-- assign key to each type in list
-- adjust counter
-- put in map and total



data Node_AndTy = Node_AndTy
  { _childCountMap ∷ HMS.HashMap MatchKey Int
  , _nodeMap       ∷ HMS.HashMap MatchKey KeyType
  }

-- Tree. Is current key node or final state
data AndKey = KeyFinal | KeyRef MatchKey





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
  



lookupAndTy ∷ KeySet → Node_AndTy → KeySet

