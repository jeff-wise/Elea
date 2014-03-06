

module Elea.Lang.Atom.Index.Val (
    ValIndex    
  , newValIndex
  , lookup, insert
  ) where


import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Atom.Val


import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.List.Stream as L
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T




---------------------------------------------------------------------
-- 1.0 Types
---------------------------------------------------------------------

---------------------------------------------------------------------
-- 1.1 Match Keys
---------------------------------------------------------------------

type MatchKey = Int
type KeySet = Set.Set MatchKey




---------------------------------------------------------------------
-- 1.2 Value Index
---------------------------------------------------------------------
--
data ValIndex = ValIndex
  { _valNode        ∷ Node_Val
  , _valMap         ∷ HMS.HashMap MatchKey Val
  , _valKeyCounter  ∷ Int
  }




---------------------------------------------------------------------
-- 1.3 Index Nodes
---------------------------------------------------------------------
--
data Node_Val = Node_Val
  { _node_Dict  ∷  Node_Dict
  , _node_Arr   ∷  Node_Arr
  , _node_Set   ∷  Node_Set
  , _node_Text  ∷  Node_Text
  , _node_Num   ∷  Node_Num
  }


data Node_Dict = Node_Dict
  { _dictNodeHM   ∷ HMS.HashMap T.Text Node_Val
  , _dictSizeMap  ∷ HMS.HashMap MatchKey Int
  } 


data Node_Set = Node_Set
  { _setKeyMap  ∷  HMS.HashMap MatchKey MatchKey
  , _setSizeMap ∷  HMS.HashMap MatchKey Int
  , _setValNode ∷  Node_Val
  , _setCounter ∷  Int
  }


data Node_Arr = Node_Arr
  { _arrNodeSeq ∷ Seq.Seq Node_Val }


data Node_Num = Node_Num
  { _numMap ∷  Map.Map Number KeySet }


data Node_Text = Node_Text
  { _textMap  ∷  HMS.HashMap Text KeySet }




---------------------------------------------------------------------
-- 1.4 Showable Index
---------------------------------------------------------------------

instance Show ValIndex where
  show (ValIndex valNode valItemMap valKeyCounter) =
    "ValNode:\n " ++ show valNode ++ "\n" ++
    "ItemMap:\n " ++ show valItemMap ++ "\n" ++
    "Counter:\n " ++ show valKeyCounter ++ "\n"
 

instance Show Node_Val where 
  show nodeVal =
    "Set:\n " ++ (show $ _node_Set nodeVal) ++ "\n" ++
    "Arr:\n " ++ (show $ _node_Arr nodeVal) ++ "\n" ++ 
    "Num:\n " ++ (show $ _node_Num nodeVal) ++ "\n" ++
    "Text:\n " ++ (show $ _node_Text nodeVal) ++ "\n" ++
    "Sym:\n " ++ (show $ _node_Sym nodeVal) ++ "\n"


instance Show Node_Dict where
  show (Node_Dict dictHM) = show dictHM


instance Show Node_Arr where
  show (Node_Arr arrSeq) = show arrSeq


instance Show Node_Set where
  show (Node_Set keyMap _ valNode _) =
    "KeyMap:\n " ++ show keyMap ++ "\n"  ++
    "ValNode:\n " ++ show valNode ++ "\n" 


instance Show Node_Num where
  show (Node_Num numMap) = show numMap


instance Show Node_Text where
  show (Node_Text textMap) = show textMap



---------------------------------------------------------------------
-- 1.5 Lenses
---------------------------------------------------------------------

makeLenses ''Node_Val




---------------------------------------------------------------------
-- 1.6 Constructors
---------------------------------------------------------------------

newValIndex ∷ ValIndex
newValIndex = ValIndex
  { _valNode        =  newValNode
  , _valMap         =  HMS.empty
  , _valKeyCounter  =  0
  }


-- | Lazily construct a value node
newValNode ∷ Node_Val
newValNode = Node_Val
  { _node_Dict  = Node_Dict HMS.empty
  , _node_Arr   = Node_Arr Seq.empty
  , _node_Set   = Node_Set HMS.empty HMS.empty newValNode 0
  , _node_Text  = Node_Text HMS.empty
  , _node_Num   = Node_Num Map.empty
  }




---------------------------------------------------------------------
-- 2.0 Insert
---------------------------------------------------------------------

-- Values are verified by the system to be unique before inserted.
insert ∷ Val → ValIndex → ValIndex
insert val (ValIndex valNode valItemMap valKeyCounter) =
  ValIndex {
    _valNode       = insertVal val valKeyCounter valNode
  , _valMap        = HMS.insert valKeyCounter val valItemMap
  , _valKeyCounter = valKeyCounter + 1
  }



insertVal ∷ Val → MatchKey → Node_Val → Node_Val

insertVal (Val_Dict dict) key node  = node & node_Dict %~ insertDict dict  key

insertVal (Val_Arr  arr ) key node  = node & node_Arr  %~ insertArr  arr  key

insertVal (Val_Set  set ) key node  = node & node_Set  %~ insertSet  set key

insertVal (Val_Text text) key node  = node & node_Text %~ insertText text key

insertVal (Val_Num  num ) key node  = node & node_Num  %~ insertNum  num  key

insertVal _               _   node  = node




insertDict ∷ Dict → MatchKey → Node_Dict → Node_Dict
insertDict (Dict insHM) matchKey (Node_Dict dictNodeHM) =
  let updateDict nodeHM insKey insVal =
        case HMS.lookup insKey nodeHM of
          Just valNode  →
            HMS.insert insKey (insertVal insVal matchKey valNode)
          Nothing       →
            HMS.insert insKey (insertVal insVal matchKey newValNode)
  in  Node_Dict 
      { _dictNodeHM  = HMS.foldlWithKey' updateDict dictNodeHM insHM
      , _dictSizeMap = HMS.insert matchKey (HMS.size insHM)
      }
    

insertArr ∷ Array → MatchKey → Node_Arr → Node_Arr
insertArr (Arr arr) key (Node_Arr nodeSeq) =
  Node_Arr $ go (viewl arr) (viewl nodeSeq)
  where
    go EmptyL           EmptyL                   = Seq.empty
    go EmptyL           (valNode :< remValNodes) = valNode <| remValNodes
    go (val :< remVals) EmptyL                   = 
      fmap (\x → insertVal x key newValNode) (val <| remVals)
    go (val :< remVals) (valNode :< remValNodes) =
      insertVal val key valNode <| go (viewl remVals) (viewl remValNodes)



insertSet ∷ Set → MatchKey → Node_Set → Node_Set
insertSet (Set set) key (Node_Set keyMap sizeMap valNode counter) =
  let counter'  = counter + setSize
      sizeMap'  = HMS.insert key setSize sizeMap
      valNode'  = L.foldl' addSetElemToNode valNode
                      (HS.toList set `L.zip` setKeys)
      keyMap'   = L.foldl' addElemKeyToMap keyMap setKeys
  in  Node_Set keyMap' sizeMap' valNode' counter'
  where
    setSize   = HS.size set
    setKeys   = [counter..(counter + setSize -1)]
    addSetElemToNode node (elem, elemKey) = insertVal elem elemKey node
    addElemKeyToMap currKeyMap elemKey = HMS.insert elemKey key currKeyMap



insertText ∷ Text → MatchKey → Node_Text → Node_Text
insertText text key (Node_Text textMap) = Node_Text $
  HMS.insertWith Set.union text (Set.singleton key) textMap



insertNum ∷ Number → MatchKey → Node_Num → Node_Num
insertNum num key (Node_Num numMap) = Node_Num $
  Map.insertWith Set.union num (Set.singleton key) numMap




---------------------------------------------------------------------
-- 3.0 Lookup
---------------------------------------------------------------------

lookup ∷ Type → ValIndex → HS.HashSet Val
lookup ty (ValIndex valNode valMap _) =
  let matchKeys = Set.toList $ lookupVal ty valNode
  in  HS.fromList $ (flip fmap) matchKeys (\matchKey →
        case HMS.lookup matchKey valMap of
          Just val → val
          Nothing  → error "Should not happen"
      )



lookupVal ∷ Type → Node_Val → KeySet

lookupVal (Ty_Dict dictTy) node  = lookupDict dictTy $ _node_Dict node

lookupVal (Ty_Arr  arrTy ) node  = lookupArr  arrTy  $ _node_Arr  node

lookupVal (Ty_Set  setTy ) node  = lookupSet  setTy  $ _node_Set  node

lookupVal (Ty_And  andTy ) node  = lookupAnd  andTy node

lookupVal (Ty_Or   orTy  ) node  = lookupOr   orTy  node

lookupVal (Ty_Text textTy) node  = lookupText textTy $ _node_Text node 

lookupVal (Ty_Num  numTy ) node  = lookupNum  numTy  $ _node_Num  node

lookupVal (Ty_Any        ) node  = (lookupSet  AnySet    $ _node_Set  node)
                       `Set.union` (lookupArr  AnyArray  $ _node_Arr  node)
                       `Set.union` (lookupText AnyText   $ _node_Text node)
                       `Set.union` (lookupNum  AnyNumber $ _node_Num  node)
                       `Set.union` (lookupSym  AnySymbol $ _node_Sym  node)




lookupDict ∷ DictTy → Node_Dict → KeySet

lookupDict (HasEntry key ty  ) (Node_Dict nodeHM) = 
  case HMS.lookup key nodeHM of
    Just valNode  →  lookupVal ty valNode
    Nothing       →  Set.empty

lookupDict (IsDict entryTypes) (Node_Dict nodeHM sizeMap) =
  let collKeys Nothing        _         = Nothing
      collKeys (Just keyMap) (key, ty) = do
        valNode ← HMS.lookup key nodeHM
        let matchKeys = lookupVal ty valNode
            insMatchKey hm key = HMS.insertWith (+) key 1 hm
        return $ Just $ L.foldr insMatchKey keyMap
                        . lookupVal ty valNode
      filterTotalMatches matchKeys (key, found) =
        case HMS.lookup key sizeMap of
          Just size →  if size == found
                          then insert key matchKeys
                          else matchKeys
          Nothing   →  matchKeys 
  in      entryTypes
      >$> L.foldr collKeys (Just HMS.empty)
      >>> HMS.foldl' filterTotalMatches Set.empty




lookupArr ∷ ArrayTy → Node_Arr → KeySet

lookupArr (WithIndex idx ty) (Node_Arr nodeArr) =
  if (idx >= 0) && (idx < Seq.length nodeArr)
    then lookupVal ty $ Seq.index nodeArr idx
    else Set.empty

lookupArr (IsArray  tySeq ) (Node_Arr nodeArr) =
  let go _                EmptyL                    = Set.empty
      go EmptyL           _                         = Set.empty
      go (ty :< remTys)   (valNode :< remValNodes)
        | Seq.null remTys = lookupVal ty valNode
        | otherwise       = Set.intersection
                              (lookupVal ty valNode)
                              (go (viewl remTys) (viewl remValNodes))
  in  go (viewl tySeq) (viewl nodeArr)

lookupArr AnyArray           (Node_Arr nodeArr) =
    F.foldl' Set.union Set.empty $
      fmap (lookupVal Ty_Any) nodeArr




lookupSet ∷ SetTy → Node_Set → KeySet

lookupSet (WithElem    elemTy) (Node_Set setMap _ valNode _      ) =
  let elemKeys = lookupVal elemTy valNode
      collectSetKeys setKeys elemKey = 
        setKeys `Set.union` 
        (maybe Set.empty Set.singleton $ HMS.lookup elemKey setMap)
  in Set.foldl' collectSetKeys Set.empty elemKeys
{-
lookupSet (IsSet setTys      ) (Node_Set setMap sizeMap valNode _) =
  let unionElemMatches matchKeys nextElemTy = 
        matchKeys `Set.union` lookupVal nextElemTy valNode
      countSetMatches matchMap elemKey =
        case HMS.lookup elemKey setMap of
          Just setKey → Map.insertWith (+) setKey 1 matchMap
          Nothing     → error "Should not happen (refactor)"
      filterTotalMatches matches setKey keyCount =
        case HMS.lookup setKey sizeMap of
          Just setSize  →  if setSize == keyCount
                              then Set.insert setKey matches
                              else matches
          Nothing       →  error "should not happen (refactor)"
  in      setTys  
      >$> HS.foldl' unionElemMatches Set.empty         
      >>> Set.foldl' countSetMatches Map.empty
      >>> Map.foldlWithKey' filterTotalMatches Set.empty
-}
lookupSet (SetWithSize num   ) (Node_Set _ sizeMap _ _) =
  let size = maybe 0 id $ asInt num
  in  if size > 0
        then  Set.fromList $ HMS.keys $
                HMS.filter (==size) sizeMap
        else Set.empty

lookupSet AnySet                  (Node_Set _ sizeMap _ _) =
  Set.fromList $ HMS.keys sizeMap




lookupAnd ∷ AndTy → Node_Val → KeySet
lookupAnd (AndTy ty1 ty2) valNode =
  lookupVal ty1 valNode `Set.intersection` lookupVal ty2 valNode




lookupOr ∷ OrTy → Node_Val → KeySet
lookupOr (OrTy ty1 ty2) valNode =
  lookupVal ty1 valNode `Set.union` lookupVal ty2 valNode




lookupText ∷ TextTy → Node_Text → KeySet

lookupText (WithTextLen len ) (Node_Text textMap) =
  let unionWhenLen keySet text keys =
        Set.union keySet $
          if T.length (_getText text) == len
           then keys
           else Set.empty
  in  HMS.foldlWithKey' unionWhenLen Set.empty textMap

lookupText (IsText      text) (Node_Text textMap) =
  maybe Set.empty id $ HMS.lookup text textMap

lookupText AnyText            (Node_Text textMap) =
  HMS.foldl' Set.union Set.empty textMap




lookupNum ∷ NumberTy → Node_Num → KeySet

lookupNum (IsNumber     num       ) (Node_Num numMap) =
    maybe Set.empty id $ Map.lookup num numMap

lookupNum (GreaterThan  lowerBound) (Node_Num numMap) =
  Map.foldl' Set.union Set.empty $
    snd $ Map.split lowerBound numMap 

lookupNum (LessThan     upperBound) (Node_Num numMap) =
  Map.foldl' Set.union Set.empty $
    fst $ Map.split upperBound numMap 

lookupNum (InRange  x  y          ) (Node_Num numMap) =
    let xKeys = maybe Set.empty id $ Map.lookup x numMap
        yKeys = maybe Set.empty id $ Map.lookup y numMap
        btwKeys = Map.foldl' Set.union Set.empty $
                    fst $ Map.split y $ snd $ Map.split x numMap
    in  xKeys `Set.union` btwKeys `Set.union` yKeys

lookupNum Even                      (Node_Num numMap) =
    Map.foldl' Set.union Set.empty $
      Map.filterWithKey (\k _ → numEven k) numMap

lookupNum Odd                       (Node_Num numMap) =
    Map.foldl' Set.union Set.empty $
      Map.filterWithKey (\k _ → numOdd k) numMap

lookupNum Integer                   (Node_Num numMap) =
    Map.foldl' Set.union Set.empty $
      Map.filterWithKey (\k _ → isInteger k) numMap

lookupNum NonNegative               (Node_Num numMap) =
    Map.foldl' Set.union Set.empty $
      Map.filterWithKey (\k _ → k >= 0) numMap

lookupNum AnyNumber                (Node_Num numMap) =
    Map.foldl' Set.union Set.empty numMap



