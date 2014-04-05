

module Elea.Lang.Index.Type (
    TypeIndex
  , newTypeIndex
  , insert, lookup
  ) where


import Elea.Prelude

import Elea.Lang.Exp.Type
import Elea.Lang.Exp.Value


import Control.Monad.State.Lazy --(State, runState, get, modify)


import qualified Data.Foldable as F
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import qualified Data.List.Stream as L
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T




---------------------------- TYPES -------------------------


-- | A key is a certificate for some type.
-- If a value matches a type at some type node, then
-- that type node should return the corresponding key.
type TypeKey = Int


-- | A set of keys
-- A keyset at a terminating node indicates that
-- its path intersects multiple types
type KeySet = Set.Set TypeKey


-- | Increment counter to ensure unique key generation
type KeyCounter = Int


-- | State Monad to track unique keys.
type NodeUpdate a = State KeyCounter a



-- | Type Index
data TypeIndex = TypeIndex
  { _tyNode       ∷ Node_Type
  , _tyMap        ∷ HMS.HashMap TypeKey Type
  , _tyKeyCounter ∷ KeyCounter
  }



-- | Type Node
-- Merges multiple types into one large union type.
-- See the explanation of 'ValueIndex'. The Type Index
-- works under the same principles.
--
-- Intersection types have multiple member types which must
-- each be satisfied in order to satisfy the type. We must
-- track these member types as premises, using unique keys,
-- which can be used to infer the conclusion key. 
-- 
-- A key which is not a premise of any other keys is an
-- input/final key.
data Node_Type =
  Node_Type
  { recordTypeNode  ∷  Node_RecordType
  , arrayTypeNode   ∷  Node_ArrayType
  , textTypeNode    ∷  Node_TextType
  , numberTypeNode  ∷  Node_NumberType
  , premiseMap      ∷  PremiseMap
  }



-- | Record Type Node
data Node_RecordType =
  Node_RecordType
  { hasEntryTypeIndex   ∷  HMS.HashMap T.Text Node_Type
  , recOfSizeTypeIndex  ∷  HMS.HashMap Int KeySet
  , anyRecordTypeIndex  ∷  KeySet
  }




-- | Array Type Node
data Node_ArrayType =
  Node_ArrayType
  { withIndexTypeIndex  ∷  HMS.HashMap Int Node_Type 
  , arrOfSizeTypeIndex  ∷  HMS.HashMap Int KeySet 
  , anyArrayTypeIndex   ∷  KeySet
  }



-- | Text Type Node
data Node_TextType =
  Node_TextType
  { withTextLenTypeIndex  ∷  HMS.HashMap Int KeySet
  , isTextTypeIndex       ∷  HMS.HashMap Text KeySet
  , anyTextTypeIndex      ∷  KeySet
  }



-- | Number Type Node
data Node_NumberType =
  Node_NumberType
  { isNumberTypeIndex     ∷  Map.Map Number KeySet
  , greaterThanTypeIndex  ∷  Map.Map Number KeySet
  , lessThanTypeIndex     ∷  Map.Map Number KeySet
  , rangeTypeIndex        ∷  ( Map.Map Number KeySet
                              , Map.Map Number KeySet)
  , integerTypeIndex      ∷  KeySet
  , nonNegativeTypeIndex  ∷  KeySet
  , anyNumberTypeIndex    ∷  KeySet
  }



-- | Premise Map
-- Stores premise keys.
data PremiseMap =
  PremiseMap 
    -- | Maps premise keys to conclusion keys
  { inferenceMap    ∷ HMS.HashMap TypeKey TypeKey
    -- | Indicates number of premise keys required for
    -- some conclusion key.
  , premiseCountMap ∷ HMS.HashMap TypeKey Int
  }




------------------------ CONSTRUCTORS ----------------------

-- | Create a new Type Index
newTypeIndex ∷ TypeIndex
newTypeIndex = TypeIndex
  { _tyNode       = newTypeNode
  , _tyMap        = HMS.empty
  , _tyKeyCounter = 0
  }


-- | Lazily construct a new Type Node
newTypeNode ∷ Node_Type
newTypeNode =
  Node_Type
  { recordTypeNode  = Node_RecordType
                      { hasEntryTypeIndex     = HMS.empty
                      , recOfSizeTypeIndex    = HMS.empty
                      , anyRecordTypeIndex    = Set.empty
                      }
  , arrayTypeNode   = Node_ArrayType
                      { withIndexTypeIndex    = HMS.empty
                      , arrOfSizeTypeIndex    = HMS.empty
                      , anyArrayTypeIndex     = Set.empty
                      }
  , textTypeNode    = Node_TextType 
                      { withTextLenTypeIndex  = HMS.empty
                      , isTextTypeIndex       = HMS.empty
                      , anyTextTypeIndex      = Set.empty
                      }
  , numberTypeNode  = Node_NumberType
                      { isNumberTypeIndex     = Map.empty
                      , greaterThanTypeIndex  = Map.empty
                      , lessThanTypeIndex     = Map.empty
                      , rangeTypeIndex        = (Map.empty, Map.empty)
                      , integerTypeIndex      = Set.empty
                      , nonNegativeTypeIndex  = Set.empty
                      , anyNumberTypeIndex    = Set.empty
                      }
  , premiseMap      = PremiseMap HMS.empty HMS.empty
  }




--------------------------- INSERT -------------------------

-- | Insert a type into the index.
-- Currently, this function does not check for duplicate types.
insert ∷ Type → TypeIndex → TypeIndex
insert ty (TypeIndex tyNode tyMap currKey) =
  let (tyNode', nextKey) = runState (insertType ty tyNode) currKey
  in  TypeIndex {
        _tyNode       = tyNode'
      , _tyMap        = HMS.insert currKey ty tyMap
      , _tyKeyCounter = nextKey
      }




-- | Insert a type into the correct node
insertType ∷ Type → Node_Type → NodeUpdate Node_Type
insertType ty tyNode@(Node_Type
                    currRecordTypeNode
                    currArrayTypeNode
                    currTextTypeNode
                    currNumberTypeNode
                    _         ) = 

  case ty of

    Ty_Rec  recTy   → do
      newRecordTypeNode ← insertRecordType recTy currRecordTypeNode
      return $ tyNode { recordTypeNode = newRecordTypeNode }

    Ty_Arr  arrTy   → do
      newArrayTypeNode ← insertArrayType arrTy currArrayTypeNode
      return $ tyNode { arrayTypeNode = newArrayTypeNode }

    Ty_And  andTy   → insertAndType andTy tyNode

    Ty_Or   orTy    → insertOrType orTy tyNode

    Ty_Text textTy  → do
      newTextTypeNode ← insertTextType textTy currTextTypeNode
      return $ tyNode { textTypeNode = newTextTypeNode }

    Ty_Num  numTy   → do
      newNumberTypeNode ← insertNumberType numTy currNumberTypeNode
      return $ tyNode { numberTypeNode = newNumberTypeNode }




-- | Insert a Record Type
insertRecordType ∷ RecordType → Node_RecordType
                    → NodeUpdate Node_RecordType
insertRecordType recordType recTyNode@(Node_RecordType
                                  currHasEntryTypeIndex
                                  currRecOfSizeTypeIndex
                                  currAnyRecordTypeIndex ) =

  case recordType of

    HasEntry label ty →  do
      newHasEntryTypeIndex ← updateTypeNodeMap label ty currHasEntryTypeIndex
      return recTyNode {
        hasEntryTypeIndex = newHasEntryTypeIndex
      }

    RecOfSize size    →  do
      case size < 1 of
        True  →  do
          key ← get
          return recTyNode {
            recOfSizeTypeIndex =
              updateKeySetHashMap key size currRecOfSizeTypeIndex            
          }
        -- No records exist of zero or negative size
        False →  return recTyNode

    AnyRecord       → do
      key ← get
      return recTyNode {
        anyRecordTypeIndex = Set.insert key currAnyRecordTypeIndex
      }




insertArrayType ∷ ArrayType → Node_ArrayType → NodeUpdate Node_ArrayType
insertArrayType arrayType arrTyNode@(Node_ArrayType
                                 currWithIndexTypeIndex                               
                                 currArrOfSizeTypeIndex  
                                 currAnyArrayTypeIndex  ) = 

  case arrayType of

    WithIndex index ty  →  do
      case index >= 0 of
        -- Index is valid
        True  →  do
          newWithIndexTypeIndex ← updateTypeNodeMap index ty currWithIndexTypeIndex
          return arrTyNode {
            withIndexTypeIndex = newWithIndexTypeIndex
          }
        -- Invalid Index 
        False →  return arrTyNode

    ArrOfSize size      →  do
      case size > 0 of
        True  →  do
          key ← get
          return arrTyNode {
            arrOfSizeTypeIndex = 
              updateKeySetHashMap key size currArrOfSizeTypeIndex            
          }
        False →  return arrTyNode

    AnyArray            →  do
      key ← get
      return arrTyNode {
        anyArrayTypeIndex = Set.insert key currAnyArrayTypeIndex
      }





-- | An Or type is an untagged union of types, so it may
-- be indexed simply by inserting each type of the union
-- into the node using the same key.
--
-- When a lookup returns that key, then THERE EXISTS some type
-- in the union type that matches the given value.
insertOrType ∷ OrType → Node_Type → NodeUpdate Node_Type
insertOrType (OrType tyList) tyNode =
  F.foldrM insertType tyNode tyList




-- | An And type is an intersection type. Unlike Or types, we
-- must track each member type separately, to ensure that a given
-- lookup value matches FOR ALL types in the type set.
--
-- This algorithm creates a new key for each type and inserts
-- that key into the node. The new key is indicated as a premise
-- key (in the Premise Key Map), since it will be used as proof
-- to imply the validity of the input (conclusion) key.
insertAndType ∷ AndType → Node_Type → NodeUpdate Node_Type
insertAndType (AndType tyList) andTyNode = do
  key ← get
  -- Insert each type
  andTyNode' ← F.foldrM (insertAndType' key) andTyNode tyList
  -- Store number of premises for input key
  let (PremiseMap infMap preCountMap) = premiseMap andTyNode'
  return andTyNode' {
    premiseMap =  PremiseMap infMap $
                    HMS.insert key (L.length tyList) preCountMap
  }

  where

    -- Insert a type with a new key, add new key
    -- as premise to final key
    insertAndType' ∷ TypeKey → Type → Node_Type → NodeUpdate Node_Type
    insertAndType' conclusionKey ty tyNode = do
      modify (+1)     -- Update key counter
      nextKey ← get ∷ NodeUpdate KeyCounter
      let (PremiseMap infMap preCountMap) = premiseMap tyNode
          infMap' = HMS.insert nextKey conclusionKey infMap
          tyNode' = tyNode {
                      premiseMap = PremiseMap infMap' preCountMap
                    }
      insertType ty tyNode'
                  



insertTextType ∷ TextType → Node_TextType → NodeUpdate Node_TextType
insertTextType textType textTyNode@(Node_TextType
                                  currWithTextLenTypeIndex
                                  currIsTextTypeIndex
                                  currAnyTextTypeIndex ) = 

  case textType of

    WithTextLen len →  do
      case len > 0 of
        -- Text length must be postive integer
        True  →  do
          key ← get
          return textTyNode {
            withTextLenTypeIndex =
              updateKeySetHashMap key len currWithTextLenTypeIndex
          }
        -- If length is negative, return unmodified index
        False →  return textTyNode

    IsText text     →  do
      key ← get
      return textTyNode {
        isTextTypeIndex = updateKeySetHashMap key text currIsTextTypeIndex
      }

    AnyText         → do
      key ← get
      return textTyNode {
        anyTextTypeIndex = Set.insert key currAnyTextTypeIndex
      }





insertNumberType ∷ NumberType → Node_NumberType
                    → NodeUpdate Node_NumberType
insertNumberType numberType numTyNode@(Node_NumberType 
                                  currIsNumberTypeIndex
                                  currGreaterThanTypeIndex
                                  currLessThanTypeIndex
                                  currRangeTypeIndex
                                  currIntegerTypeIndex
                                  currNonNegativeTypeIndex
                                  currAnyNumberTypeIndex  ) =

  case numberType of

    IsNumber    num →  do
      key ← get
      return numTyNode {
        isNumberTypeIndex = updateKeySetMap key num currIsNumberTypeIndex
      }

    GreaterThan num →  do
      key ← get
      return numTyNode {
        greaterThanTypeIndex =
          updateKeySetMap key num currGreaterThanTypeIndex
      }

    LessThan    num →  do
      key ← get
      return numTyNode {
        lessThanTypeIndex = updateKeySetMap key num currLessThanTypeIndex
      }

    InRange lb ub   →  do
      key ← get
      let (currLbMap, currUbMap) = currRangeTypeIndex
      return numTyNode {
        rangeTypeIndex = ( updateKeySetMap key lb currLbMap
                         , updateKeySetMap key ub currUbMap )
      }

    Integer         →  do 
      key ← get
      return numTyNode {
        integerTypeIndex = Set.insert key currIntegerTypeIndex
      }

    NonNegative     → do
      key ← get
      return numTyNode {
        nonNegativeTypeIndex = Set.insert key currNonNegativeTypeIndex
      }

    AnyNumber       → do
      key ← get
      return numTyNode {
        anyNumberTypeIndex = Set.insert key currAnyNumberTypeIndex
      }





--------------------------- LOOKUP -------------------------

lookup ∷ Value → TypeIndex → [Type]
lookup value (TypeIndex tyNode tyMap _) =
  let getItem key = fromJust $ HMS.lookup key tyMap
  in  L.map getItem $ Set.toList $ lookupTy value tyNode




lookupTy ∷ Value → Node_Type → KeySet
lookupTy value tyNode = deriveKeys tyNode typeKeys
  where
    typeKeys =
      case value of
        (Val_Rec  rec ) → lookupRecTy  rec  $ recordTypeNode tyNode
        (Val_Arr  arr ) → lookupArrTy  arr  $ arrayTypeNode tyNode
        (Val_Text text) → lookupTextTy text $ textTypeNode tyNode
        (Val_Num  num ) → lookupNumTy  num  $ numberTypeNode tyNode




lookupRecTy ∷ Record → Node_RecordType → KeySet
lookupRecTy (Rec rec) (Node_RecordType
                       currHasEntryTypeIndex
                       currRecOfSizeTypeIndex
                       currAnyRecordTypeIndex ) =

    -- Union keys of all possible types for this value
                hasEntryTypeKeys
    `Set.union` recOfSizeTypeKeys
    `Set.union` anyRecordTypeKeys

  where

    -- | For each entry in the record, check if there are any
    -- stored types which describe that entry
    hasEntryTypeKeys =
      let consEntryTypes accKeys atLabel forValue =
            Set.union accKeys $ entryTypes atLabel forValue
      in  HMS.foldlWithKey' consEntryTypes Set.empty rec
      where
        entryTypes label entryVal = 
          let mEntryTypeNode = HMS.lookup label currHasEntryTypeIndex
          in  case mEntryTypeNode of
                Just    tyNode → lookupTy entryVal tyNode
                Nothing        → Set.empty

    -- | Lookup types describing record of this record's size
    recOfSizeTypeKeys  =
      let mKeys = HMS.lookup (HMS.size rec) currRecOfSizeTypeIndex
      in  maybe Set.empty id mKeys

    -- | Find types of any record
    anyRecordTypeKeys = currAnyRecordTypeIndex




-- | Find all types at this node which describe the array
lookupArrTy ∷ Array → Node_ArrayType → KeySet
lookupArrTy (Arr arr) (Node_ArrayType
                       currWithIndexTypeIndex                               
                       currArrOfSizeTypeIndex  
                       currAnyArrayTypeIndex  ) = 

  -- Union keys of all possible types for this value
              withIndexTypeKeys 
  `Set.union` arrOfSizeTypeKeys
  `Set.union` anyArrayTypeKeys
    
  where
  
    -- | For each index in the array, see if value at that index
    -- matches types for that particular index at this node
    withIndexTypeKeys = 
      let addIndexTypeKeys accKeys index arrElem = 
            let mTyNode = HMS.lookup index currWithIndexTypeIndex
                keysAtIndex = case mTyNode of
                                Just tyNode → lookupTy arrElem tyNode
                                Nothing     → Set.empty
            in  accKeys `Set.union` keysAtIndex
      in  Seq.foldlWithIndex addIndexTypeKeys Set.empty arr

    -- | Lookup types describing arrays of this array's size
    arrOfSizeTypeKeys = 
      let mKeys = HMS.lookup (Seq.length arr) currArrOfSizeTypeIndex
      in  maybe Set.empty id mKeys

    -- | Find array types
    anyArrayTypeKeys = currAnyArrayTypeIndex




-- | Find all types at this node which describe some text
lookupTextTy ∷ Text → Node_TextType → KeySet
lookupTextTy text (Node_TextType
                   currWithTextLenTypeIndex
                   currIsTextTypeIndex
                   currAnyTextTypeIndex ) = 

  -- Union keys of all possible types for this value
              withTextLenTypeKeys
  `Set.union` isTextTypekeys
  `Set.union` anyTextTypeKeys

  where

    withTextLenTypeKeys =
      let mTextLenKeys = HMS.lookup (T.length $ primText text)
                          currWithTextLenTypeIndex
      in  maybe Set.empty id mTextLenKeys

    isTextTypekeys =
      let mIsTextKeys = HMS.lookup text currIsTextTypeIndex
      in  maybe Set.empty id mIsTextKeys
    
    anyTextTypeKeys = currAnyTextTypeIndex





lookupNumTy ∷ Number → Node_NumberType → KeySet
lookupNumTy num  (Node_NumberType 
                  currIsNumberTypeIndex
                  currGreaterThanTypeIndex
                  currLessThanTypeIndex
                  currRangeTypeIndex
                  currIntegerTypeIndex
                  currNonNegativeTypeIndex
                  currAnyNumberTypeIndex  ) =

  -- Union keys of all possible types for this value
              isNumberTypeKeys
  `Set.union` greaterThanTypeKeys
  `Set.union` lessThanTypeKeys
  `Set.union` rangeTypeKeys
  `Set.union` integerTypeKeys
  `Set.union` nonNegativeTypeKeys
  `Set.union` anyNumberTypeKeys

  where

    isNumberTypeKeys    = maybe Set.empty id $
                            Map.lookup num currIsNumberTypeIndex

    greaterThanTypeKeys = Map.foldl' Set.union Set.empty $
                            fst $ Map.split num currGreaterThanTypeIndex

    lessThanTypeKeys    = Map.foldl' Set.union Set.empty $
                            snd $ Map.split num currLessThanTypeIndex

    rangeTypeKeys       =
      let (lbMap, ubMap)  = currRangeTypeIndex
          aboveLB         = Map.foldl' Set.union Set.empty $
                              fst $ Map.split num lbMap
          areLB           = maybe Set.empty id $ Map.lookup num lbMap
          belowUB         = Map.foldl' Set.union Set.empty $
                              snd $ Map.split num ubMap
          areUB           = maybe Set.empty id $ Map.lookup num ubMap
      in  (aboveLB `Set.union` areLB) `Set.intersection` 
          (belowUB `Set.union` areUB)

    integerTypeKeys     = case num of
                            Z _ → currIntegerTypeIndex
                            _   → Set.empty

    nonNegativeTypeKeys = case num >= (Z 0) of
                            True  → currNonNegativeTypeIndex
                            False → Set.empty
      
    anyNumberTypeKeys   = currAnyNumberTypeIndex
      
  



-------------------------- APPENDIX ------------------------
------------------------------------------------------------


------------------------- DERIVE KEYS ----------------------


-- | Part of derivation.
-- Sort keys into a map with tracks the number of instances
-- of premise keys found for some conclusion key, and
-- the number of final keys found
type KeyAnalysis = (HMS.HashMap TypeKey Int, KeySet) 
newKeyAnalysis = (HMS.empty, Set.empty)


-- | Given a set of keys found at some node, return the
-- input keys by evaluating the keys as premises until no
-- more conclusions are inferred
deriveKeys ∷ Node_Type → KeySet → KeySet
deriveKeys tyNode premiseKeys = stepUntilDone premiseKeys Set.empty
  where
    stepUntilDone ∷ KeySet → KeySet → KeySet
    stepUntilDone keysToEval finalKeysFound
      | Set.null keysToEval = finalKeysFound
      | otherwise         =
          let (newKeysToEval, newFinalKeys) = step tyNode keysToEval
          in  stepUntilDone newKeysToEval
                (finalKeysFound `Set.union` newFinalKeys)


-- | One derivation step
-- Evaluate the given keys. If they are premises, attempt
-- to infer conclusions, and if cannot, discard them.
-- If they are final keys, accumulate and return them,
-- along with new conclusions, to be further evaluated.
step ∷ Node_Type → KeySet → (KeySet, KeySet)
step tyNode keysToEval =
  let (preFoundMap, finalKeysFound) =
        Set.foldl' consKeyAnalysis newKeyAnalysis keysToEval
      keysToEval' = HMS.foldlWithKey' consInferences Set.empty preFoundMap
  in  (keysToEval', finalKeysFound)

  where

    -- | Rule: Construct set of conclusions
    -- Premises:
    --  * Set of final keys
    --  * Some premises (actually, a count of premises)
    --      for some key inference
    -- Conclusion:
    --  * A set of final keys. If the a conclusion
    --    could be derived from the premises, then the
    --    new set will include a new conclusion key
    consInferences ∷ KeySet → TypeKey → Int → KeySet
    consInferences keySet conKey premisesFound = 
      let (PremiseMap _ preCountMap) =  premiseMap tyNode
          numPremisesReq = fromJust $ HMS.lookup conKey preCountMap
      in  if numPremisesReq == premisesFound
            then Set.insert conKey keySet
            else keySet


    -- | Rule: Construct Key Analysis
    consKeyAnalysis ∷ KeyAnalysis → TypeKey → KeyAnalysis
    consKeyAnalysis (conKeyCountMap, currFinalKeys) key = 
      let (PremiseMap inferMap _) =  premiseMap tyNode
      in  case HMS.lookup key inferMap of
            -- Key is a premise, update conclusion key map 
            Just conKey →
              ( HMS.insertWith (+) conKey 1 conKeyCountMap
              , currFinalKeys
              )
            -- Key is final, add to list
            Nothing     →
              (conKeyCountMap, Set.insert key currFinalKeys) 




----------------------- INDEX UPDATES ----------------------


updateTypeNodeMap ∷ (Eq a, Hashable a) ⇒
                      a → Type → HMS.HashMap a Node_Type
                      → State KeyCounter (HMS.HashMap a Node_Type)
updateTypeNodeMap mapKey newType nodeMap = do
  case HMS.lookup mapKey nodeMap of
    Just tyNode →  do
      tyNode' ← insertType newType tyNode
      return $ HMS.insert mapKey tyNode' nodeMap
    Nothing     →  do
      newNode ← insertType newType newTypeNode
      return $ HMS.insert mapKey newNode nodeMap


updateKeySetHashMap ∷ (Eq a, Hashable a) ⇒
                       TypeKey → a → HMS.HashMap a KeySet
                       → HMS.HashMap a KeySet
updateKeySetHashMap matchKey mapKey keysetMap =  
    HMS.insertWith
      Set.union             -- Union old keyset with new keyset
      mapKey
      (Set.singleton matchKey)   -- Insert new key as set
      keysetMap


updateKeySetMap ∷ (Ord a) ⇒ TypeKey → a → Map.Map a KeySet
                    → Map.Map a KeySet
updateKeySetMap matchKey mapKey keysetMap =  
    Map.insertWith
      Set.union             -- Union old keyset with new keyset
      mapKey
      (Set.singleton matchKey)   -- Insert new key as set
      keysetMap


