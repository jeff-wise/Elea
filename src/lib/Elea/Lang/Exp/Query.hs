

module Elea.Lang.Exp.Query (query) where



import Elea.Prelude
import Elea.Lang.Atom.Types
import qualified Elea.Lang.Atom.Index.Val as VI
import Elea.Lang.Exp.Types


--import qualified Data.List.Stream as L
--import qualified Data.HashMap.Strict as HMS
--import qualified Data.HashSet as HS



query ∷ Context → Query → STM Val
query (Ctx _ sysVar triggerVal) q = runQuery q
  where
    runQuery (Query_Const val) = return val
    runQuery (Query_Ctx      ) = (view sysVal) <$>
                                      (readTVar sysVar)
    runQuery (Query_Type  ty ) = queryByTy sysVar ty
--    runQuery (Query_Rel   relQuery) = queryByRel sysVar relQuery
    runQuery (Query_Trig     ) = return triggerVal
  


{-
queryByRel ∷ TVar System → RelQuery → STM Val
queryByRel sysVar (RelQuery initRelId initDomTy compRels) = do
  system ← readTVar sysVar
  relMap ← readTVar $ system^.sysRelMap
  childIndex ← readTVar $ system^.sysChildIndex
  let initDomElems = VI.lookup initDomTy childIndex
      codElems = related relMap initDomElems initRelId
  return $ Val_Set $ Set $
    L.foldl' (related relMap) codElems compRels
  where
    related relMap domElems relId =
      fromMaybe HS.empty $ do 
        relation ← HMS.lookup relId relMap 
        let unionRels accSet domElem =
              case HMS.lookup domElem relation of
                Just codSet → accSet `HS.union` codSet
                Nothing     → accSet
        return $ HS.foldl' unionRels HS.empty domElems

-}  
 


queryByTy ∷ TVar System → Type → STM Val
queryByTy sysVar queryTy =
      readTVar sysVar
  >>= (view sysChildIndex >>> readTVar)
  >>= (     VI.lookup queryTy
        >>> Set 
        >>> Val_Set
        >>> return 
      )
        

