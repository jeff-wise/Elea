

module Elea.Lang.Exp.Universe (
    findSystem
  , report
  , newUniverse
  ) where



import Elea.Prelude
import Elea.Lang.Atom.Types
import Elea.Lang.Exp.Types
import qualified Elea.Lang.Atom.Index.Val as VI


import Control.Monad.State.Lazy

import qualified Data.Foldable as F
import qualified Data.List.Stream as L
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Text as T






newUniverse ∷ System → STM Universe
newUniverse mainSystem = do
  program ← readTVar $ mainSystem^.sysProgram
  let (program', symbolTable) =
        runState (compProgram program) newSymbolTable
  symTableVar ← newTVar symbolTable
  writeTVar (mainSystem^.sysProgram) program'
  univ ← Universe
            <$> (newTVar mainSystem)
            <*> (return symTableVar)
  return univ




compVal ∷ Val → State SymbolTable Val
compVal (Val_Set (Set set )) =
  fmap (Val_Set . Set . HS.fromList) $ 
    mapM compVal (HS.toList set)
compVal (Val_Arr  (Arr arr )) =
  fmap (Val_Arr . Arr . Seq.fromList) $
    mapM compVal (F.toList arr)
compVal (Val_Loc  locVal    ) =
  case locVal of
    (Loc_Sym symText) → compSymbol symText
compVal leafVal               = return leafVal



-- TODO refactor with pipes?
compProgram ∷ Program → State SymbolTable Program
compProgram (Program ruleMap ruleIdx) = do
  ruleMap' ← updateRuleMap ruleMap
  return $ Program ruleMap' ruleIdx
  where
    updateRuleMap  = fmap HMS.fromList $ 
      forM (HMS.toList ruleMap) (\(ty, actionMap) →
        let actionMap' =
              forM (HMS.toList actionMap) (\(actId, actionList) →
                (,) actId <$> compActions actionList
              )
        in  (,) ty <$> compActions actions
      )



compActions ∷ [Action] → State SymbolTable [Action]
compActions actions = compActions' actions (return [])
  where
    compActions' []                  stActions = stActions
    compActions' (action:remActions) stActions =
      case action of 
        (Action_Con con) →
          let compAction = Action_Con <$> compCon con
              stActions' = (:) <$> compAction <*> stActions
          in  compActions' remActions stActions'
   


compCon ∷ Constructor → State SymbolTable Constructor
compCon (Con initActions prog synth loc) =
  let updateProgram     = compProgram prog
      updateInitActions = compActions initActions
      updateSynth       = do
        synT' ← compVal $ synth^.synTemplate
        return $ synth & synTemplate .~ synT'
  in  Con <$> updateInitActions
          <*> updateProgram
          <*> updateSynth
          <*> (return loc)



---------------------------------------------------------------------
-- Create Symbols
---------------------------------------------------------------------
{-
defSymbol ∷ TVar SymbolTable → T.Text → STM Symbol
defSymbol symTableVar symText = do
  symbolTable ← readTVar symTableVar
  let (sym, symbolTable') = compSymbol symText symbolTable
  writeTVar symTableVar symbolTable' 
  return sym
-}


compSymbol ∷ T.Text → State SymbolTable Val
compSymbol symText = state compSymbol'
  where
    compSymbol' table@(SymTable count symMap) =
      case L.find ((symText==) . snd) $ HMS.toList symMap of
        -- Symbol already defined, return it
        Just (symId, _) → (Val_Sym $ Sym symId, table)
        -- Create a new int mapping for text symbol
        Nothing         →
            let update = over tblCount (+1)
                     >>> over tblMap (HMS.insert count symText)
            in  (Val_Sym $ Sym count, update table)



---------------------------------------------------------------------
-- Find Systems
---------------------------------------------------------------------
   

report ∷ Val → STM ()
report _ = return ()


