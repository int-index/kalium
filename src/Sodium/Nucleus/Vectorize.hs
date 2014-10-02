{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..)) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Nucleus.Program.Scalar
import qualified Sodium.Nucleus.Program.Vector as Vec

data Error
    = NoAccess Name Vec.Indices
    | NoFunction Operator
    | UpdateImmutable Name
    | NoReference
    | InvalidOperation
    deriving (Show)

type E = Except Error
type R m a = ReaderT Vec.Indices m a
type S m a = StateT  Vec.Indices m a

vectorize :: Program -> E Vec.Program
vectorize program = do
    let funcSigs = program ^.. programFuncs . traversed . funcSig
    vecFuncs <- mapM (vectorizeFunc funcSigs) (program ^. programFuncs)
    return $ Vec.Program vecFuncs

references :: FuncSig -> [Name]
references funcSig = do
    (name, (by, _)) <- funcSig ^. funcParams
    guard (by == ByReference)
    return name

vectorizeFunc :: [FuncSig] -> Func -> E Vec.Func
vectorizeFunc funcSigs func = do
    (_, vecBodyGen)
            <- runReaderT (vectorizeBody funcSigs (func ^. funcBody))
            $ initIndices (Vec.Index 0) (func ^. funcSig . funcParams . to M.fromList)
    let refs = map Access (func ^. funcSig . to references)
    vecBody <- vecBodyGen (func ^. funcResults ++ refs)
    return $ Vec.Func (func ^. funcSig) vecBody

vectorizeBody :: [FuncSig] -> Body -> R E ([Name], [Expression] -> E Vec.Body)
vectorizeBody funcSigs body = do
    closure <- ask
    lift $ do
        let isLocal = flip elem (body ^. bodyVars . to M.keys)
        let indices = initIndices Vec.Uninitialized (body ^. bodyVars) `M.union` closure
        (vecStatements, indices')
                <- flip runStateT indices
                 $ mapM (vectorizeStatement' funcSigs) (body ^. bodyStatements)
        let changed
                = M.keys
                $ M.filterWithKey ((&&) . not . isLocal)
                $ M.intersectionWith (/=)
                indices indices'
        let vecBodyGen results
                = Vec.Body (body ^. bodyVars) (map (uncurry Vec.Bind) vecStatements)
                <$> runReaderT (mapM vectorizeExpression results) indices'
        return (changed, vecBodyGen)

vectorizeBody' :: [FuncSig] -> Body -> R E ([Name], Vec.Body)
vectorizeBody' funcSigs body = do
    (changed, vecBodyGen) <- vectorizeBody funcSigs body
    vecBody <- lift $ vecBodyGen (map Access changed)
    return (changed, vecBody)

vectorizeStatement' :: [FuncSig] -> Statement -> S E (Vec.IndicesList, Vec.Statement)
vectorizeStatement' funcSigs statement
    = _1 (mapM registerIndexUpdate)
    =<< readerToState (vectorizeStatement funcSigs statement)

vectorizeStatement :: [FuncSig] -> Statement -> R E ([Name], Vec.Statement)
vectorizeStatement funcSigs = \case
    BodyStatement body
         -> over _2 Vec.BodyStatement
        <$> vectorizeBody' funcSigs body
    Execute mres name args -> do
        vecArgs <- mapM vectorizeExpression args
        let byReference (_, (ByReference, _)) = \case
              Vec.Access name _ -> Just [name]
              _                 -> Nothing
            byReference (_, (ByValue, _)) = const (Just [])
        funcSig <- lift (lookupFuncSig funcSigs name)
        sidenames <- case zipWith byReference (funcSig ^. funcParams) vecArgs
                          & sequence
                     of Nothing -> throwError NoReference
                        Just ns -> return (concat ns)
        let resnames = maybe empty pure mres

        -- TODO: purity flag in function signature
        let impure = case name of
              OpReadLn _ -> True
              OpPrintLn  -> True
              _ -> False
        let vecExecute
              | impure    = Vec.Execute name vecArgs
              | otherwise = Vec.Assign (Vec.Call name vecArgs)
        return $ (resnames ++ sidenames, vecExecute)
    ForStatement forCycle -> over _2 Vec.ForStatement <$> do
        vecRange <- vectorizeExpression (forCycle ^. forRange)
        (changed, vecBody)
            <- local (M.insert (forCycle ^. forName) Vec.Immutable)
             $ vectorizeBody' funcSigs (forCycle ^. forBody)
        argIndices <- closedIndices changed
        let vecForCycle = Vec.ForCycle
                argIndices
                (uncurry Vec.Access `map` argIndices)
                (forCycle ^. forName)
                vecRange
                (Vec.BodyStatement vecBody)
        return (changed, vecForCycle)
    MultiIfStatement multiIfBranch -> over _2 Vec.MultiIfStatement <$> do
        (unzip -> (changedList, vecLeafGens))
            <- forM (multiIfBranch ^. multiIfLeafs)
             $ \(expr, body) -> do
                 vecExpr <- vectorizeExpression expr
                 (changed, vecBodyGen) <- vectorizeBody funcSigs body
                 let vecLeafGen = \results -> do
                         vecBody <- vecBodyGen results
                         return (vecExpr, Vec.BodyStatement vecBody)
                 return (changed, vecLeafGen)
        (changedElse, vecBodyElseGen)
            <- vectorizeBody funcSigs (multiIfBranch ^. multiIfElse)
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
       & maybe (throwError $ NoAccess name indices) return

lookupFuncSig :: [FuncSig] -> Operator -> E FuncSig
lookupFuncSig funcSigs op@(OpName name)
    = find (\funcSig -> view funcName funcSig == name) funcSigs
    & maybe (throwError $ NoFunction op) return
-- TODO: real signatures for builtin operators
lookupFuncSig _ _ = return $ FuncSig (Name "") [] (TypeUnit)

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

initIndices :: Vec.Index -> M.Map Name a -> Vec.Indices
initIndices n = M.map (const n)
