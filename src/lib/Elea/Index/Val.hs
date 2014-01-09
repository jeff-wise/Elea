

module Elea.Index.Val (
    newValIndex
  , ValIndex    
  , lookup
  , insert
  , _valNode
  , _node_Arr
  ) where


import Elea.Prelude
import Elea.Lang.Types
import Elea.Lang.Val


import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T



---------------------------------------------------------------------
-- Value Index Types
---------------------------------------------------------------------

type MatchKey = Int
type KeySet = Set.Set MatchKey


data ValIndex a = ValIndex
  { _valNode    ∷ Node_Val
  , _itemMap    ∷ HMS.HashMap MatchKey a
  , _keyCounter ∷ Int
  }



data Node_Val = 
    Empty 
  | Node_Val
      { _node_Set   ∷  Node_Set
      , _node_Arr   ∷  Node_Arr
      , _node_Text  ∷  Node_Text
      , _node_Num   ∷  Node_Num
      , _node_Sym   ∷  Node_Sym
      }



data Node_Set = Node_Set
  { _setSizeMap ∷  HMS.HashMap MatchKey Int  -- | Lengths of stored sets
  , _setValNode ∷  Node_Val                  -- | Set values
  }


data Node_Arr = Node_Arr
  { _arrNodeSeq ∷ Seq.Seq Node_Val }


data Node_Num = Node_Num
  { _numMap ∷  Map.Map Number KeySet }


data Node_Text = Node_Text
  { _textMap  ∷  HMS.HashMap Text KeySet }


data Node_Sym = Node_Sym 
  { _symMap   ∷  HMS.HashMap Symbol KeySet }


-- Val Node instances

instance (Show a) ⇒ Show (ValIndex a) where
  show (ValIndex valNode itemMap keyCounter) =
    "ValNode:\n " ++ show valNode ++ "\n" ++
    "ItemMap:\n " ++ show itemMap ++ "\n" ++
    "Counter:\n " ++ show keyCounter ++ "\n"
 

instance Show Node_Val where 
  show Empty   = "Empty"
  show nodeVal =
    "Set:\n " ++ (show $ _node_Set nodeVal) ++ "\n" ++
    "Arr:\n " ++ (show $ _node_Arr nodeVal) ++ "\n" ++ 
    "Num:\n " ++ (show $ _node_Num nodeVal) ++ "\n" ++
    "Text:\n " ++ (show $ _node_Text nodeVal) ++ "\n" ++
    "Sym:\n " ++ (show $ _node_Sym nodeVal) ++ "\n"


instance Show Node_Set where
  show (Node_Set keyMap valNode) =
    "KeyMap:\n " ++ show keyMap ++ "\n"  ++
    "ValNode:\n " ++ show valNode ++ "\n" 


instance Show Node_Arr where
  show (Node_Arr arrSeq) = show arrSeq


instance Show Node_Num where
  show (Node_Num numMap) = show numMap


instance Show Node_Text where
  show (Node_Text textMap) = show textMap


instance Show Node_Sym where
  show (Node_Sym symMap) = show symMap




---------------------------------------------------------------------
-- Lenses
---------------------------------------------------------------------

makeLenses ''Node_Val




---------------------------------------------------------------------
-- Constructors
---------------------------------------------------------------------

newValIndex ∷ ValIndex a
newValIndex = ValIndex
  { _valNode     =  newValNode
  , _itemMap     =  HMS.empty
  , _keyCounter  =  0
  }



-- | Lazily construct a value node
newValNode ∷ Node_Val
newValNode = Node_Val
  { _node_Set   = Node_Set HMS.empty Empty
  , _node_Arr   = Node_Arr Seq.empty
  , _node_Text  = Node_Text HMS.empty
  , _node_Num   = Node_Num Map.empty
  , _node_Sym   = Node_Sym HMS.empty
  }




---------------------------------------------------------------------
-- Insert
---------------------------------------------------------------------

insert ∷ Val → a → ValIndex a → ValIndex a
insert val item (ValIndex valNode itemMap keyCounter) =
  ValIndex {
    _valNode    = insertVal val keyCounter valNode
  , _itemMap    = HMS.insert keyCounter item itemMap
  , _keyCounter = keyCounter + 1
  }



insertVal ∷ Val → MatchKey → Node_Val → Node_Val
insertVal val             key Empty = insertVal val key newValNode
insertVal (Val_Set  set ) key node  = node & node_Set  %~ insertSet  set key
insertVal (Val_Arr  arr ) key node  = node & node_Arr  %~ insertArr  arr  key
insertVal (Val_Text text) key node  = node & node_Text %~ insertText text key
insertVal (Val_Num  num ) key node  = node & node_Num  %~ insertNum  num  key
insertVal (Val_Sym  sym ) key node  = node & node_Sym  %~ insertSym  sym  key
insertVal _               _   node  = node



-- Set represents a single val that with multiple values
-- Insert each set item into the val node
insertSet ∷ Set → MatchKey → Node_Set → Node_Set
insertSet (Set set) key (Node_Set keyMap valNode) =
  let keyMap'  = HMS.insert key (HS.size set) keyMap
      valNode' = HS.foldl' addValToNode valNode set
  in  Node_Set keyMap' valNode'
  where
    addValToNode node  val = insertVal val key node



--insertPair ∷ Pair → MatchKey → Node_Pair → Node_Pair
--insertPair (Pair a b) key (Node_Pair nodeA nodeB) =
--  Node_Pair (insertVal a key nodeA) (insertVal b key nodeB)



insertArr ∷ Array → MatchKey → Node_Arr → Node_Arr
insertArr (Arr arr) key (Node_Arr nodeSeq) =
  let go EmptyL           EmptyL                   = Seq.empty
      go EmptyL           (valNode :< remValNodes) = valNode <| remValNodes
      go (val :< remVals) EmptyL                   = 
        fmap (\x → insertVal x key newValNode) (val <| remVals)
      go (val :< remVals) (valNode :< remValNodes) =
        insertVal val key valNode <| go (viewl remVals) (viewl remValNodes)
  in  Node_Arr $ go (viewl arr) (viewl nodeSeq)



insertText ∷ Text → MatchKey → Node_Text → Node_Text
insertText text key (Node_Text textMap) = Node_Text $
  HMS.insertWith Set.union text (Set.singleton key) textMap


insertNum ∷ Number → MatchKey → Node_Num → Node_Num
insertNum num key (Node_Num numMap) = Node_Num $
  Map.insertWith Set.union num (Set.singleton key) numMap



insertSym ∷ Symbol → MatchKey → Node_Sym → Node_Sym
insertSym sym key (Node_Sym symMap) = Node_Sym $
  HMS.insertWith Set.union sym (Set.singleton key) symMap




---------------------------------------------------------------------
-- Lookup
---------------------------------------------------------------------


lookup ∷ Type → ValIndex a → [a]
lookup ty (ValIndex valNode itemMap _) =
  let matchKeys = Set.toList $ lookupVal ty valNode
  in  (flip fmap) matchKeys (\matchKey →
        case HMS.lookup matchKey itemMap of
          Just item → item
          Nothing   → error "Should not happen"
      )



lookupVal ∷ Type → Node_Val → KeySet
lookupVal _                Empty = Set.empty
lookupVal (Ty_Set  setTy ) node  = lookupSet  setTy  $ _node_Set  node
lookupVal (Ty_Arr  arrTy ) node  = lookupArr  arrTy  $ _node_Arr  node
lookupVal (Ty_And  andTy ) node  = lookupAnd  andTy node
lookupVal (Ty_Or   orTy  ) node  = lookupOr   orTy  node
lookupVal (Ty_Text textTy) node  = lookupText textTy $ _node_Text node 
lookupVal (Ty_Num  numTy ) node  = lookupNum  numTy  $ _node_Num  node
lookupVal (Ty_Sym  symTy ) node  = lookupSym  symTy  $ _node_Sym  node
lookupVal (Ty_Any        ) node  = (lookupSet  AnySet    $ _node_Set  node)
                       `Set.union` (lookupArr  AnyArray  $ _node_Arr  node)
                       `Set.union` (lookupText AnyText   $ _node_Text node)
                       `Set.union` (lookupNum  AnyNumber $ _node_Num  node)
                       `Set.union` (lookupSym  AnySymbol $ _node_Sym  node)




lookupSet ∷ SetTy → Node_Set → KeySet
lookupSet (WithElem    elemTy  ) (Node_Set _      valNode) =
  lookupVal elemTy valNode
--lookupSet (WithoutElem elemTy  ) (Node_Set setMap valNode) =
--  (Set.fromList $ HMS.keys setMap) `Set.difference`
--  lookupVal elemTy valNode 
lookupSet (SetWithSize num     ) (Node_Set setMap _      ) =
  let size = case num of
              (Z i) → i
              (R d) → maybe (-1) id $ doubleToInt d
  in  if size < 0
        then Set.empty
        else Set.fromList $ HMS.keys $ HMS.filter (==size) setMap
--lookupSet (IsSet       tyList  ) (Node_Set _      valNode) =
--  L.foldl' Set.intersection Set.empty $
--    L.map (flip lookupVal $ valNode) tyList
lookupSet AnySet                 (Node_Set setMap _      ) =
  Set.fromList $ HMS.keys setMap




lookupArr ∷ ArrayTy → Node_Arr → KeySet
lookupArr (WithIndex num ty) (Node_Arr nodeArr) =
  let idx = case num of
              (Z i) → i
              (R d) → maybe (-1) id $ doubleToInt d
  in  if (idx >= 0) && (idx < Seq.length nodeArr)
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



lookupAnd ∷ AndTy → Node_Val → KeySet
lookupAnd (AndTy ty1 ty2) valNode =
  lookupVal ty1 valNode `Set.intersection` lookupVal ty2 valNode



lookupOr ∷ OrTy → Node_Val → KeySet
lookupOr (OrTy ty1 ty2) valNode =
  lookupVal ty1 valNode `Set.union` lookupVal ty2 valNode



lookupText ∷ TextTy → Node_Text → KeySet
lookupText (WithTextLen (Z len)) (Node_Text textMap) =
  let unionWhenLen keySet text keys =
        Set.union keySet $ if T.length (_getText text) == len
                             then keys
                             else Set.empty
  in  HMS.foldlWithKey' unionWhenLen Set.empty textMap
lookupText (IsText      text   ) (Node_Text textMap) =
  maybe Set.empty id $ HMS.lookup text textMap
lookupText AnyText               (Node_Text textMap) =
  HMS.foldl' Set.union Set.empty textMap
lookupText _                         _                   = Set.empty



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



lookupSym ∷ SymbolTy → Node_Sym → KeySet
lookupSym (IsSymbol sym) (Node_Sym symMap) =
  maybe Set.empty id $ HMS.lookup sym symMap
lookupSym AnySymbol      (Node_Sym symMap) =
  HMS.foldl' Set.union Set.empty $ symMap
   

