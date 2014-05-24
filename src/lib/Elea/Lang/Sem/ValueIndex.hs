

-- | Value Index
--
-- A datastructure which stores values and automatically indexes specific
-- properties so that they may be queried by 'Type'. 
module Elea.Lang.Sem.ParticleIndex (
    ParticleIndex    
  , newParticleIndex
  , lookup, insert
  ) where


import Elea.Prelude

import Elea.Lang.Term.Value
import Elea.Lang.Term.Type


import Data.Foldable as F
import Data.Hashable
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.Map as Map
import Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T


-- need to return values matched, actually need particles matched
-- need to map value to many particles


-------------------------- TYPES --------------------------


-- | A key which identifies a particular type.
-- If a value matches a type at some type node, then
-- that type node should return the corresponding key.
type TypeKey = Int


-- | A set of keys
-- A keyset at a terminating node indicates that
-- its path intersects multiple types
type KeySet = Set.Set TypeKey


-- | Increment counter to ensure unique key generation
type KeyCounter = Int


-- | Particle Index
data ParticleIndex = ParticleIndex
  { indexValueNode  ∷ Node_Value
  , indexValueMap   ∷ HMS.HashMap TypeKey Particle
  , indexKeyCounter ∷ KeyCounter
  }



-- | Value Node
-- Merges all values into one data structure.
-- The match keys identify the nodes of some embedded
-- tree. 
--
-- Example: Stored values of A, B, C, and D
--
--                    [A-B-C-D]
--                  /     |      \
--               [A-B]    C     [A-B-C-D]*
--              /                  \
--           [A-B]                [B-C]
--
--  * In order to examine values A and B, an algorithm needs
--    to only traverse those left and right paths once.
--  * If the values of A, B, C, and D at the starred node are
--    all of the same type, then they can be stored in an optimal
--    data structure. If one wanted to find which of A, B, C, or D
--    at that node was equal to 5, the lookup could be performed
--    on an ordered map (binary tree) very quickly, as opposed
--    to linearly by examine the node at each tree separately.
--
-- Each node indexes a value by different attributes, so that
-- searches for values with specific properties can on average
-- proceed very fast. In essence, we are throwing space/memory
-- away to get fast lookups.
--
-- By default the Value Index is always in memory, so space may
-- be an issue. Later, there will be settings to tune the amount
-- and/or methods of indexing. Automatic optimizations are always
-- possible, e.g. if a certain type is never planned to be used,
-- then its associated properties do not need to be indexed.


data Node_Value = Node_Value
  { recordNode  ∷  Node_Record
  , arrayNode   ∷  Node_Array
  , textNode    ∷  Node_Text
  , numberNode  ∷  Node_Number
  }


data Node_Record = Node_Record
  { entryIndex      ∷ HMS.HashMap T.Text Node_Value
  , recordSizeIndex ∷ HMS.HashMap Int KeySet
  } 


data Node_Array = Node_Array
  { elementIndex      ∷ Seq.Seq Node_Value
  , arrayLengthIndex  ∷ HMS.HashMap Int KeySet
  }


data Node_Text = Node_Text 
  { textIndex       ∷ HMS.HashMap Text KeySet
  , textLengthIndex ∷ HMS.HashMap Int KeySet
  }


newtype Node_Number = Node_Number
  { numberIndex   ∷ Map.Map Number KeySet }





------------------------ CONSTRUCTORS ----------------------

newParticleIndex ∷ ParticleIndex
newParticleIndex = ParticleIndex
  { indexValueNode  =  newValueNode
  , indexValueMap   =  HMS.empty
  , indexKeyCounter =  0
  }


-- | Lazily construct a value node
newValueNode ∷ Node_Value
newValueNode = Node_Value
  { recordNode  = Node_Record HMS.empty HMS.empty
  , arrayNode   = Node_Array Seq.empty HMS.empty
  , textNode    = Node_Text HMS.empty HMS.empty
  , numberNode  = Node_Number Map.empty
  }




--------------------------- INSERT -------------------------

-- Values are verified by the system to be unique before inserted.
insert ∷ Value → a → ParticleIndex → ParticleIndex
insert value (ParticleIndex currValueNode valueMap currKey) =
  ParticleIndex
  { indexValueNode  = insertValue value currKey currValueNode
  , indexValueMap   = HMS.insert currKey value valueMap
  , indexKeyCounter = currKey + 1
  }



insertValue ∷ Value → TypeKey → Node_Value → Node_Value
insertValue value key valNode@(Node_Value
                               currRecordNode
                               currArrayNode
                               currTextNode
                               currNumberNode ) = 

  case value of
    Val_Rec rec →  valNode {
                        recordNode  = insertRecord rec key currRecordNode
                      }
    Val_Arr arr →  valNode {
                        arrayNode   = insertArray  arr key currArrayNode
                      } 
    Val_Txt txt →  valNode {
                        textNode    = insertText   txt key currTextNode
                      }
    Val_Num num →  valNode {
                        numberNode  = insertNumber num key currNumberNode
                      }




insertRecord ∷ Record → TypeKey → Node_Record → Node_Record
insertRecord (Rec rec) key
             (Node_Record currEntryIndex currRecordSizeIndex) =
  let insertEntry accEntryIndex label entryValue =
              HMS.insertWith
                (\_ currEntryValueNode → 
                    insertValue entryValue key currEntryValueNode)
                label
                (insertValue entryValue key newValueNode)
                accEntryIndex
  in  Node_Record 
      { entryIndex      = HMS.foldlWithKey' insertEntry currEntryIndex rec
      , recordSizeIndex =
          updateKeySetHashMap key (HMS.size rec) currRecordSizeIndex
      }
    


insertArray ∷ Array → TypeKey → Node_Array → Node_Array
insertArray (Arr arrSeq) key
            (Node_Array currElementIndex currArrayLengthIndex) =
  Node_Array
      { elementIndex      = updateElementIndex arrSeq currElementIndex
      , arrayLengthIndex  =
          updateKeySetHashMap key (Seq.length arrSeq) currArrayLengthIndex
      }

  where

    updateElementIndex seq elemIndex = go (viewl seq) (viewl elemIndex)
      where
        go EmptyL           EmptyL                   = Seq.empty
        go EmptyL           (valNode :< remValNodes) = valNode <| remValNodes
        go (val :< remVals) EmptyL                   = 
          fmap (\x → insertValue x key newValueNode) (val <| remVals)
        go (val :< remVals) (valNode :< remValNodes) =
          insertValue val key valNode <| go (viewl remVals) (viewl remValNodes)




insertText ∷ Text → TypeKey → Node_Text → Node_Text
insertText text key (Node_Text currTextIndex currTextLengthIndex) =
  Node_Text
    { textIndex       = updateKeySetHashMap key text currTextIndex
    , textLengthIndex =
        updateKeySetHashMap key (T.length $ primText text) currTextLengthIndex  
    }




insertNumber ∷ Number → TypeKey → Node_Number → Node_Number
insertNumber number key (Node_Number currNumberIndex) =
  Node_Number {
    numberIndex =  updateKeySetMap key number currNumberIndex
  }



-------------------------- LOOKUP --------------------------

-- | Lookup values by property.
lookup ∷ Type → ParticleIndex → HS.HashSet Value
lookup typ (ParticleIndex valueNode valueMap _) =
  let typeKeys = Set.toList $ lookupType typ valueNode
  in  HS.fromList $ (flip fmap) typeKeys (\typeKey →
        case HMS.lookup typeKey valueMap of
          Just value  → value
          Nothing     → error "Should not happen"
      )



lookupType ∷ Type → Node_Value → KeySet
lookupType typ valueNode = 

  case typ of
    Ty_Rec  recTy → lookupRecordType recTy $ recordNode valueNode
    Ty_Arr  arrTy → lookupArrayType  arrTy $ arrayNode  valueNode
    Ty_And  andTy → lookupAndType    andTy valueNode
    Ty_Or   orTy  → lookupOrType     orTy  valueNode
    Ty_Txt  txtTy → lookupTextType   txtTy $ textNode   valueNode
    Ty_Num  numTy → lookupNumberType numTy $ numberNode valueNode
    Ty_Any        → allNodeValues

  where

    allNodeValues = (lookupRecordType AnyRecord $ recordNode valueNode)
        `Set.union` (lookupArrayType  AnyArray  $ arrayNode valueNode)
        `Set.union` (lookupTextType   AnyText   $ textNode valueNode)
        `Set.union` (lookupNumberType AnyNumber $ numberNode valueNode)




lookupRecordType ∷ RecordType → Node_Record → KeySet
lookupRecordType recordType
                (Node_Record currEntryIndex currRecordSizeIndex) =

  case recordType of

    HasEntry label entryType  →
      case HMS.lookup label currEntryIndex of
        Just entryValueNode → lookupType entryType entryValueNode
        Nothing             →  Set.empty

    WithSize size             →
      case HMS.lookup size currRecordSizeIndex of
        Just typeKeys → typeKeys
        Nothing       → Set.empty

    AnyRecord                 →
                  (HMS.foldl' consNodeMapKeySets Set.empty currEntryIndex)
      `Set.union` (allIndexKeys currRecordSizeIndex)




lookupArrayType ∷ ArrayType → Node_Array → KeySet
lookupArrayType arrayType
                (Node_Array currElementIndex currArrayLengthIndex) =

  case arrayType of

    WithIndex idx typ →
      if (idx >= 0) && (idx < Seq.length currElementIndex)
        then lookupType typ $ Seq.index currElementIndex idx
        else Set.empty

    WithArrLen len    →
      case HMS.lookup len currArrayLengthIndex of 
        Just keys → keys
        Nothing   → Set.empty

    AnyArray          →
                  (F.foldl' consNodeMapKeySets Set.empty currElementIndex)
      `Set.union` (allIndexKeys currArrayLengthIndex)



-- | Lookup values that satisfy intersection type.
-- Here we literally take the intersection of all the values
-- which satisfy each AND member type.
-- I.e. we provide a list of criteria that a value must meet
-- to be selected, and only take values which meet each
-- requirement.
lookupAndType ∷ AndType → Node_Value → KeySet
lookupAndType (AndType tyList) valueNode =
  F.foldr1 Set.intersection $
    fmap (\typ → lookupType typ valueNode) tyList




-- | Lookup values that satisfy union type.
-- Here we literally take the union of all the values
-- which satisfy each OR member type.
-- I.e. we provide a list of optional properties, and take
-- any value that has at least one of those properties.
lookupOrType ∷ OrType → Node_Value → KeySet
lookupOrType (OrType tyList) valueNode =
  F.foldl' Set.union Set.empty $
    fmap (\typ → lookupType typ valueNode) tyList





lookupTextType ∷ TextType → Node_Text → KeySet
lookupTextType textType
               (Node_Text currTextIndex currTextLengthIndex) =

  case textType of

    IsText      text  →
      case HMS.lookup text currTextIndex of
        Just keySet → keySet
        Nothing     → Set.empty

    WithTextLen len   →
      case HMS.lookup len currTextLengthIndex of
        Just keySet → keySet
        Nothing     → Set.empty

    AnyText           →
                  (allIndexKeys currTextIndex)
      `Set.union` (allIndexKeys currTextLengthIndex)




lookupNumberType ∷ NumberType → Node_Number → KeySet
lookupNumberType numberType (Node_Number currNumberIndex) =

  case numberType of

    IsNumber    num →  case Map.lookup num currNumberIndex of
                          Just keySet → keySet
                          Nothing     → Set.empty

    GreaterThan lb  →  Map.foldl' Set.union Set.empty $
                          snd $ Map.split lb currNumberIndex 

    LessThan    ub  →  Map.foldl' Set.union Set.empty $
                          fst $ Map.split ub currNumberIndex 

    Range  lb ub    →  
      let lbKeys  = maybe Set.empty id $ Map.lookup lb currNumberIndex
          ubKeys  = maybe Set.empty id $ Map.lookup ub currNumberIndex
          btwKeys = Map.foldl' Set.union Set.empty $
                      fst $ Map.split ub $ snd $ Map.split lb currNumberIndex
      in  lbKeys `Set.union` btwKeys `Set.union` ubKeys

    AnyNumber       →  allIndexKeys currNumberIndex



-------------------------- APPENDIX ------------------------
------------------------------------------------------------


--------------------- UTILITY FUNCTIONS --------------------

updateKeySetMap ∷ (Ord a) ⇒ TypeKey → a → Map.Map a KeySet
                    → Map.Map a KeySet
updateKeySetMap typeKey mapKey keySetMap =  
    Map.insertWith
      Set.union                 -- Union old keyset with new keyset
      mapKey
      (Set.singleton typeKey)  -- Insert new key as set
      keySetMap



updateKeySetHashMap ∷ (Eq a, Hashable a) ⇒
                       TypeKey → a → HMS.HashMap a KeySet
                       → HMS.HashMap a KeySet
updateKeySetHashMap typeKey mapKey keysetMap =  
    HMS.insertWith
      Set.union             -- Union old keyset with new keyset
      mapKey
      (Set.singleton typeKey)   -- Insert new key as set
      keysetMap




consNodeMapKeySets ∷ KeySet → Node_Value → KeySet
consNodeMapKeySets accKeys valueNode =
  accKeys `Set.union` (lookupType Ty_Any valueNode)



allIndexKeys ∷ (Foldable f) ⇒ f KeySet → KeySet
allIndexKeys f = F.foldl' Set.union Set.empty f


