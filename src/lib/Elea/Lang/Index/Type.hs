

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


type NodeUpdate = State KeyCounter Node_Ty


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
  { _node_RecTy   ∷ Node_RecTy
  , _node_ArrTy   ∷ Node_ArrTy
  , _node_SetTy   ∷ Node_SetTy
  , _node_TextTy  ∷ Node_TextTy
  , _node_NumTy   ∷ Node_NumTy
  , _keyDB        ∷ KeyDB
  }


data KeyDB = KeyDB 
  { _preMap     ∷ HMS.HashMap MatchKey MatchKey
  , _preCntMap  ∷ HMS.HashMap MatchKey Int
  }


data Node_RecTy = Node_RecTy
  { _hasEntryTy   ∷ HMS.HashMap T.Text Node_Ty
  , _recOfSizeTy  ∷ HMS.HashMap Int KeySet
  , _anyDictTy    ∷ KeySet
  }


data Node_ArrTy = Node_ArrTy
  { _withIdxTy    ∷ HMS.HashMap Int Node_Ty
  , _arrOfSizeTy  ∷ HMS.HashMap Int KeySet
  , _anyArrTy     ∷ KeySet
  }


data Node_SetTy = Node_SetTy
  { _withElemTy   ∷ Node_Ty
  , _setOfSizeTy  ∷ HMS.HashMap Int KeySet
  , _anySetTy     ∷ KeySet
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
  , _keyMap       = HMS.empty
  }




--------------------------- INSERT -------------------------


insert ∷ Type → TypeIndex → TypeIndex
insert ty (TypeIndex tyNode tyMap tyKeyCounter) =
  TypeIndex {
      _tyNode       = insertTy ty tyKeyCounter tyNode
    , _tyMap        = HMS.insert tyKeyCounter ty tyMap
    , _tyKeyCounter = tyKeyCounter + 1
  }



insertTy ∷ Type → MatchKey → Node_Ty → NodeUpdate

insertTy (Ty_Rec recTy) key tyNode = return $
  tyNode & node_RecTy   %~ (insertRecTy recTy key)

insertTy (Ty_Arr  arrTy ) key tyNode = return $
  tyNode & node_ArrTy   %~ (insertArrTy  arrTy  key)

insertTy (Ty_Set  setTy ) key tyNode = return $ 
  tyNode & node_SetTy   %~ (insertSetTy  setTy  key)

insertTy (Ty_And  andTy ) key tyNode = return $
  tyNode & node_AndTy   %~ (insertAndTy  andTy  key)

insertTy (Ty_Or   orTy  ) key tyNode = return $
  tyNode & node_OrTy    %~ (insertOrTy   orTy   key)

insertTy (Ty_Text textTy) key tyNode = return $
  tyNode & node_TextTy  %~ (insertTextTy textTy key)

insertTy (Ty_Num  numTy ) key tyNode = return $
  tyNode & node_NumTy   %~ (insertNumTy  numTy  key)




insertSetTy ∷ SetTy → MatchKey → Node_SetTy → NodeUpdate

insertSetTy (WithElem    ty  ) key setTyNode = return $
  setTyNode & withElemTy %~ insertTy ty key

insertSetTy (SetOfSize size) key setTyNode = return $
  let updateSizeNode = HMS.insertWith Set.union size (Set.singleton key)
  in  if size < 0 then setTyNode
                  else setTyNode & setWithSizeTy %~ updateSizeNode

insertSetTy AnySet               key setTyNode = return $
  setTyNode & anySetTy %~ (Set.insert key)




insertArrTy ∷ ArrayTy → MatchKey → Node_Ty → NodeUpdate
insertArrTy (WithIndex idx ty) key arrTyNode = return $
  arrTyNode & withIdxTy %~ 
    HMS.insertWith 
      (\_ node → insertTy ty key node)
      idx
      (insertTy ty key newTypeNode)

insertArrTy (ArrOfSize size  ) key arrTyNode = return $
  let updateSizeNode = HMS.insertWith Set.union size (Set.singleton key)
  in  if size < 0 then arrTyNode
                  else arrTyNode & arrOfSizeTy %~ updateSizeNode

insertArrTy AnyArray           key arrTyNode = return $
  arrTyNode & anyArrTy %~ (Set.insert key)



-- | An Or type is an untagged union of types, so it may
-- be indexed simply by inserting each type of the union
-- into the node using the same key.
--
-- When a lookup returns that key, then THERE EXISTS some type
-- in the union type that matches the given value.
insertOrTy ∷ OrTy → Matchkey → Node_Ty → NodeUpdate
insertOrTy (OrTy tyList) key tyNode =
  let insOrTy node ty = insertTy ty key node
  in  L.foldl' insOrTy tyNode tyList




-- | An And type is an intersection type. Unlike Or types, we
-- must track each member type separately, to ensure that a given
-- lookup value matches FOR ALL types in the type set.
--
-- This algorithm creates a new key for each type and inserts
-- that key into the node. The new key is indicated as a premise
-- key (in the Premise Key Map), since it will be used as proof
-- to imply the validity of the input key.
insertAndTy ∷ AndTy → MatchKey → Node_Ty → NodeUpdate
insertAndTy (AndTy tyList) inputKey tyNode = do
  -- Insert each type
  foldrM insertAndTy' tyNode tyList
  -- Store number of premises for input key
  return $ tyNode & keyDB.keyPreCountMap %
            (HMS.insert inputKey $ L.length tyList)
  where
    -- Insert a type with a new key, add new key
    -- as premise to final key
    insertAndTy' ∷ Type → Node_Ty → NodeUpdate
    insertAndTy' ty currTyNode = do
      newKey ← get 
      modify (+1)
      return $ currTyNode & keyDB.keyPreMap %~
                (HMS.insert newKey inputKey)
      insertTy ty newKey currTyNode
                  



insertTextTy ∷ TextTy → MatchKey → Node_TextTy → NodeUpdate
insertTextTy (WithTextLen num ) key textTyNode = return $
  let len = case num of
              (Z i) → i
              (R d) → maybe (-1) id $ doubleToInt d
  in  if len < 0
        then textTyNode
        else textTyNode & withTextLenTy %~ 
              HMS.insertWith Set.union len (Set.singleton key)
insertTextTy (IsText      text) key textTyNode = return $
  textTyNode & isTextTy %~ 
    HMS.insertWith Set.union text (Set.singleton key)
insertTextTy AnyText            key textTyNode = return $
  textTyNode & anyTextTy %~ (Set.insert key)




insertNumTy ∷ NumberTy → MatchKey → Node_NumTy → NodeUpdate
insertNumTy (IsNumber    num) key numTyNode = return $
  numTyNode & isNumTy %~
    Map.insertWith Set.union num (Set.singleton key)
insertNumTy (GreaterThan num) key numTyNode = return $
  numTyNode & gtNumTy %~
    Map.insertWith Set.union num (Set.singleton key)
insertNumTy (LessThan    num) key numTyNode = return $
  numTyNode & ltNumTy %~
    Map.insertWith Set.union num (Set.singleton key)
insertNumTy (InRange lb ub  ) key numTyNode = return $
  let update (lbMap, ubMap) =
        ( Map.insertWith Set.union lb (Set.singleton key) lbMap
        , Map.insertWith Set.union ub (Set.singleton key) ubMap )
  in  numTyNode & rangeTy %~ update
insertNumTy Even              key numTyNode = return $
  numTyNode & evenTy %~ (Set.insert key)
insertNumTy Odd               key numTyNode = return $
  numTyNode & oddTy %~ (Set.insert key)
insertNumTy Integer           key numTyNode = return $
  numTyNode & intTy %~ (Set.insert key)
insertNumTy NonNegative       key numTyNode = return $
  numTyNode & nonNegTy %~ (Set.insert key)
insertNumTy AnyNumber         key numTyNode = return $
  numTyNode & anyNumTy %~ (Set.insert key)




--------------------------- LOOKUP -------------------------

lookup ∷ Val → TypeIndex → [Type]
lookup val (TypeIndex tyNode tyMap _) =
  let getItem key = fromJust $ HMS.lookup key tyMap
  in  L.map getItem $ Set.toList $ lookupTy val tyNode



-- | Derive consequent types from found hypothetical types

lookupTy ∷ Val → Node_Ty → KeySet
lookupTy val tyNode =
  let keySet = lookupTy' val tyNode
  in  deriveKeys tyNode keySet  
  where
    lookupTy' =
      case val of
        (Val_Rec  rec ) → lookupRecTy  rec  (tyNode ^. node_RecTy )
        (Val_Arr  arr ) → lookupArrTy  arr  (tyNode ^. node_ArrTy )
        (Val_Set  set ) → lookupSetTy  set  (tyNode ^. node_SetTy )
        (Val_Text text) → lookupTextTy text (tyNode ^. node_TextTy)
        (Val_Num  num ) → lookupNumTy  num  (tyNode ^. node_NumTy )




lookupRecTy ∷ Record → Node_RecTy → KeySet
lookupRecTy (Rec rec) recTyNode =
  let withElemsKeys = 
        let lookupElemTy = flip lookupTy $ setTyNode ^. withElemTy
            matches      = Set.fromList $ L.map lookupElemTy $ HS.toList set
        in  Set.foldl' Set.union Set.empty matches
      withSizeKeys  = maybe Set.empty id $
        HMS.lookup (HS.size set) (setTyNode ^. setWithSizeTy)
      anySetKeys    = setTyNode ^. anySetTy
  in  withElemsKeys `Set.union` withSizeKeys `Set.union` anySetKeys




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
  




-------------------------- APPENDIX ------------------------
------------------------------------------------------------


------------------------- DERIVE KEYS ----------------------


deriveKeys ∷ Node_Ty → KeySet → KeySet
deriveKeys tyNode preKeys = deriveKeys' preKeys Set.empty
  where
    deriveKeys' ∷ KeySet → KeySet → KeySet
    deriveKeys' premises found
      | Set.null premises = found
      | otherwise         =
          let (newPremises, newFound) = step tyNode premises
          in  deriveKeys' newPremises (found `Set.union` newFound)


-- state monad
-- moves btw premise sets, each time returns proved keys
-- if keyset empty return
-- step in forward chaining derivation
step ∷ Node_Ty → KeySet → (KeySet, KeySet)
step tyNode premiseKeys =
  let (countMap, finalKeys) = Set.foldl'
                                (evalKey tyNode)
                                (Map.empty, Set.empty)
                                premiseKeys
      newPreKeys = HMS.foldl'
                    (evalInfer tyNode^.keyDB.preCntMap)
                    Set.empty
                    countMap
  in  (newPreKeys, finalKeys)
      

-- | Eval key inference
-- If all premises not found, return unmodified keyset
-- otherwise add new key.
-- Works, becasue keys are unique in set.
evalInfer ∷ HMS.HashMap MatchKey Int →
               KeySet → (Key, Int) → KeySet
evalInfer keyCountMap keySet (conKey, preFound) = 
  let preNum = fromJust $ HMS.lookup key keyCountMap
  in  if preNum == preFound
        then Set.insert conKey keySet
        else keySet


-- | If a key is not in the premap then it was never a premise
-- to anything, so it is a final form
evalKey ∷ Node_Ty → (Map.Map Key Int, KeySet) → MatchKey
          → (Map.Map Key Int, KeySet) 
evalKey tyNode (keyCntMap, finalKeys) key = 
  case HMS.lookup key $ tyNode^.keyDB.preMap of
    Just consequentKey →
      ( HMS.insertWith (+1) consequentKey 1 keyCntMap
      , finalKeys
      )
    Nothing            → (keyCntMap, Set.insert key finalKeys) 


