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
    | NoFunction Name
    | UpdateImmutable Name
    | NoReference
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
    let refs = map (review _Access') (func ^. funcSig . to references)
    vecBody <- vecBodyGen (func ^. funcResult : refs)
    return $ Vec.Func (func ^. funcSig) (Vec.BodyStatement vecBody)

patTuple = Vec.PTuple . map (uncurry Vec.PAccess)
expTuple = Vec.Tuple  . map (uncurry Vec.Access)

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
                = Vec.Body (body ^. bodyVars)
                (map (\(indices, expr) -> Vec.Bind (patTuple indices) expr) vecStatements)
                <$> runReaderT (Vec.Tuple <$> mapM vectorizeExpression results) indices'
        return (changed, vecBodyGen)

vectorizeBody' :: [FuncSig] -> Body -> R E ([Name], Vec.Body)
vectorizeBody' funcSigs body = do
    (changed, vecBodyGen) <- vectorizeBody funcSigs body
    vecBody <- lift $ vecBodyGen (map (review _Access') changed)
    return (changed, vecBody)

vectorizeStatement' :: [FuncSig] -> Statement -> S E ([(Name, Vec.Index)], Vec.Statement)
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
              NameOp (OpReadLn _) -> True
              NameOp OpPrintLn    -> True
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
                (patTuple argIndices)
                (expTuple argIndices)
                (forCycle ^. forName)
                vecRange
                (Vec.BodyStatement vecBody)
        return (changed, vecForCycle)
    MultiIfStatement multiIf -> over _2 Vec.MultiIfStatement <$> do
        (unzip -> (changedList, vecLeafGens))
            <- forM (multiIf ^. multiIfLeafs)
             $ \(expr, body) -> do
                 vecExpr <- vectorizeExpression expr
                 (changed, vecBodyGen) <- vectorizeBody funcSigs body
                 let vecLeafGen = \results -> do
                         vecBody <- vecBodyGen results
                         return (vecExpr, Vec.BodyStatement vecBody)
                 return (changed, vecLeafGen)
        let changed = nub $ concat changedList
        let accessChanged = map (review _Access') changed
        vecMultiIf <- lift
             $  Vec.MultiIf
            <$> mapM ($ accessChanged) vecLeafGens
        return $ (changed, vecMultiIf)

vectorizeExpression :: Expression -> R E Vec.Expression
vectorizeExpression expr = case expr ^? _Atom of
    Nothing -> error "InvalidOperation"
    Just atom -> vectorizeAtom atom

vectorizeAtom :: Atom -> R E Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary a)
    Access name -> Vec.Access name <$> lookupIndex name

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex :: Name -> R E Vec.Index
lookupIndex name = do
    indices <- ask
    M.lookup name indices
       & maybe (throwError $ NoAccess name indices) return

lookupFuncSig :: [FuncSig] -> Name -> E FuncSig
-- TODO: real signatures for builtin operators
lookupFuncSig _ (NameOp _) = return $ FuncSig (Name [] "") [] (TypeUnit)
lookupFuncSig funcSigs name
    = find (\funcSig -> view funcName funcSig == name) funcSigs
    & maybe (throwError $ NoFunction name) return

registerIndexUpdate :: Name -> S E (Name, Vec.Index)
registerIndexUpdate name = do
    index <- readerToState (lookupIndex name) >>= indexUpdate
    modify $ M.insert name index
    return (name, index)
    where indexUpdate = \case
            Vec.Index n -> return (Vec.Index $ succ n)
            Vec.Uninitialized -> return (Vec.Index 0)
            Vec.Immutable -> throwError (UpdateImmutable name)

closedIndices :: [Name] -> R E [(Name, Vec.Index)]
closedIndices = mapM $ \name -> do
    index <- lookupIndex name
    return (name, index)

initIndices :: Vec.Index -> M.Map Name a -> Vec.Indices
initIndices n = M.map (const n)
