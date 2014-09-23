{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vectorize (vectorize) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import qualified Sodium.Nucleus.Program.Vector as Vec

data VectorizerException
    = NoAccess Name
    | UpdateImmutable Name
    | InvalidOperation
    deriving (Show)

type E = Except VectorizerException
type R m a = ReaderT Vec.Indices m a
type S m a = StateT  Vec.Indices m a

vectorize :: Program -> Vec.Program
vectorize program = fromExcept $ do
    vecFuncs <- mapM vectorizeFunc (program ^. programFuncs)
    return $ Vec.Program vecFuncs
    where fromExcept = either (error.show) id . runExcept

vectorizeFunc :: Func -> E Vec.Func
vectorizeFunc func = do
    (_, vecBodyGen)
            <- runReaderT (vectorizeBody (func ^. funcBody))
            $ initIndices (Vec.Index 0) (func ^. funcSig . funcParams)
    vecBody <- vecBodyGen (func ^. funcResults)
    return $ Vec.Func (func ^. funcSig) vecBody

vectorizeBody :: Body -> R E ([Name], [Expression] -> E Vec.Body)
vectorizeBody body = do
    closure <- ask
    lift $ do
        let isLocal = flip elem (body ^. bodyVars . to M.keys)
        let indices = initIndices Vec.Uninitialized (body ^. bodyVars) `M.union` closure
        (vecStatements, indices')
                <- flip runStateT indices
                 $ mapM vectorizeStatement' (body ^. bodyStatements)
        let changed
                = M.keys
                $ M.filterWithKey ((&&) . not . isLocal)
                $ M.intersectionWith (/=)
                indices indices'
        let vecBodyGen results
                = Vec.Body (body ^. bodyVars) (map (uncurry Vec.Bind) vecStatements)
                <$> runReaderT (mapM vectorizeExpression results) indices'
        return (changed, vecBodyGen)

vectorizeBody' :: Body -> R E ([Name], Vec.Body)
vectorizeBody' body = do
    (changed, vecBodyGen) <- vectorizeBody body
    vecBody <- lift $ vecBodyGen (map Access changed)
    return (changed, vecBody)

vectorizeStatement' :: Statement -> S E (Vec.IndicesList, Vec.Statement)
vectorizeStatement' statement
    = _1 (mapM registerIndexUpdate)
    =<< readerToState (vectorizeStatement statement)

vectorizeStatement :: Statement -> R E ([Name], Vec.Statement)
vectorizeStatement =  \case
    BodyStatement body
         -> over _2 Vec.BodyStatement
        <$> vectorizeBody' body
    SideCall res op args -> do
        vecArgs <- mapM vectorizeExpression args
        -- TODO: typecheck in order to find out
        -- what lvalues can actually get changed
        let sidenames = []
        let resnames = [res]
        return $ (resnames ++ sidenames, Vec.Assign (Vec.call op vecArgs))
    Execute mres name args -> do
        vecArgs <- mapM vectorizeExpression args
        -- TODO: typecheck in order to find out
        -- what lvalues can actually get changed
        let sidenames = []
        let resnames = maybe empty pure mres
        return $ (resnames ++ sidenames, Vec.Execute name vecArgs)
    ForStatement forCycle -> over _2 Vec.ForStatement <$> do
        vecRange <- vectorizeExpression (forCycle ^. forRange)
        (changed, vecBody)
            <- local (M.insert (forCycle ^. forName) Vec.Immutable)
             $ vectorizeBody' (forCycle ^. forBody)
        argIndices <- closedIndices changed
        let vecForCycle = Vec.ForCycle
                argIndices
                (uncurry Vec.Access `map` argIndices)
                (forCycle ^. forName)
                vecRange
                vecBody
        return (changed, vecForCycle)
    MultiIfStatement multiIfBranch -> over _2 Vec.MultiIfStatement <$> do
        (unzip -> (changedList, vecLeafGens))
            <- forM (multiIfBranch ^. multiIfLeafs)
             $ \(expr, body) -> do
                 vecExpr <- vectorizeExpression expr
                 (changed, vecBodyGen) <- vectorizeBody body
                 let vecLeafGen = \results -> do
                         vecBody <- vecBodyGen results
                         return (vecExpr, Vec.BodyStatement vecBody)
                 return (changed, vecLeafGen)
        (changedElse, vecBodyElseGen)
            <- vectorizeBody (multiIfBranch ^. multiIfElse)
        let changed = nub $ changedElse ++ concat changedList
        let accessChanged = map Access changed
        vecMultiIfBranch <- lift
             $  Vec.MultiIfBranch
            <$> mapM ($ accessChanged) vecLeafGens
            <*> fmap Vec.BodyStatement (vecBodyElseGen accessChanged)
        return $ (changed, vecMultiIfBranch)
    _ -> throwError InvalidOperation

vectorizeExpression :: Expression -> R E Vec.Expression
vectorizeExpression = \case
    Primary a -> return (Vec.Primary a)
    Access name -> Vec.Access name <$> lookupIndex name
    _ -> throwError InvalidOperation

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex :: Name -> R E Vec.Index
lookupIndex name = do
    indices <- ask
    M.lookup name indices
       & maybe (throwError $ NoAccess name) return

registerIndexUpdate :: Name -> S E (Name, Vec.Index)
registerIndexUpdate name = do
    index <- readerToState (lookupIndex name) >>= indexUpdate
    modify $ M.insert name index
    return (name, index)
    where indexUpdate = \case
            Vec.Index n -> return (Vec.Index $ succ n)
            Vec.Uninitialized -> return (Vec.Index 0)
            Vec.Immutable -> throwError (UpdateImmutable name)

closedIndices :: [Name] -> R E Vec.IndicesList
closedIndices = mapM $ \name -> do
    index <- lookupIndex name
    return (name, index)

initIndices :: Vec.Index -> Vars -> Vec.Indices
initIndices n = M.map (const n)
