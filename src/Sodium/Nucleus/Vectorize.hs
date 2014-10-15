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

import Debug.Trace

data Error
    = NoAccess Name Vec.Indices
    | NoFunction Name
    | UpdateImmutable Name
    | NoReference
    deriving (Show)

type E = Except Error
type R m a = ReaderT Vec.Indices m a
type S m a = StateT  Vec.Indices m a

vectorize :: Program Atom -> E Vec.Program
vectorize program = do
    let funcSigs = program ^.. programFuncs . traversed . funcSig
    vecFuncs <- mapM (vectorizeFunc funcSigs) (program ^. programFuncs)
    return $ Vec.Program vecFuncs

references :: [Name] -> [ByType] -> ([Name], [Type])
references params paramTypes = unzip $ do
    (name, (by, ty)) <- zip params paramTypes
    guard (by == ByReference)
    return (name, ty)

vectorizeFunc :: [FuncSig] -> Func Atom -> E Vec.Func
vectorizeFunc funcSigs func = do
    let r = vectorizeScope funcSigs (func ^. funcScope)
    (_, vecBodyGen)
            <- runReaderT r
            $ M.fromList $ map (, Vec.Index 0) (func ^. funcParams)
    let (refnames, reftypes) = references (func ^. funcParams) (func ^. funcSig . funcParamTypes)
    let vecFuncSig = Vec.FuncSig
          (func ^. funcSig . funcName)
          (map snd $ func ^. funcSig . funcParamTypes)
          (func ^. funcSig . funcRetType)
          reftypes
    vecBody <- vecBodyGen (func ^. funcResult : map Access refnames)
    return $ Vec.Func vecFuncSig (func ^. funcParams) (Vec.BodyStatement vecBody)

patTuple = Vec.PTuple . map (uncurry Vec.PAccess)
expTuple = Vec.Tuple  . map (uncurry Vec.Access)

vectorizeScope :: [FuncSig] -> Scope Atom -> R E ([Name], [Atom] -> E Vec.Body)
vectorizeScope funcSigs scope = do
    let vars = scope ^. scopeVars
    closure <- ask
    lift $ do
        let isLocal = flip elem (M.keys vars)
        let indices = initIndices Vec.Uninitialized vars `M.union` closure
        (vecStatement, indices')
                 <- (`runStateT` indices) $ do
                        (changed, vecStatement) <- readerToState (vectorizeStatement funcSigs (scope ^. scopeStatement))
                        changed' <- mapM registerIndexUpdate changed
                        return (changed', vecStatement)
        let changed
                = M.keys
                $ M.filterWithKey ((&&) . not . isLocal)
                $ M.intersectionWith (/=)
                indices indices'
        let vecBodyGen results
                = Vec.Body vars
                [(\(indices, expr) -> Vec.Bind (patTuple indices) expr) vecStatement]
                <$> runReaderT (Vec.Tuple <$> mapM vectorizeAtom results) indices'
        return (changed, vecBodyGen)


vectorizeStatement' :: [FuncSig] -> Statement Atom -> S E ([(Name, Vec.Index)], Vec.Statement)
vectorizeStatement' funcSigs statement
    = do
        (changed, vecStatement) <- readerToState (vectorizeStatement funcSigs statement)
        changed' <- mapM registerIndexUpdate changed
        return (changed', vecStatement)

vectorizeStatement :: [FuncSig] -> Statement Atom -> R E ([Name], Vec.Statement)
vectorizeStatement funcSigs = \case
    Group statements -> do
        indices <- trace "group!" ask
        lift $ do
            (vecStatements, indices')
                    <- flip runStateT indices
                     $ mapM (vectorizeStatement' funcSigs) statements
            let changed
                    = M.keys
                    $ M.filter id
                    $ M.intersectionWith (/=)
                    indices indices'
            vecBody
                    <- Vec.Body M.empty
                    (map (\(indices, expr) -> Vec.Bind (patTuple indices) expr) vecStatements)
                    <$> runReaderT (Vec.Tuple <$> mapM vectorizeAtom (map Access changed)) indices'
            return (changed, Vec.BodyStatement vecBody)
    ScopeStatement scope -> over _2 Vec.BodyStatement <$> do
        (changed, vecBodyGen) <- vectorizeScope funcSigs scope
        vecBody <- lift $ vecBodyGen (map Access changed)
        return (changed, vecBody)
    Execute (Exec mres name args) -> do
        vecArgs <- mapM vectorizeAtom args
        let byReference (ByReference, _) = \case
              Vec.Access name _ -> Just [name]
              _                 -> Nothing
            byReference (ByValue, _) = const (Just [])
        funcSig <- lift (lookupFuncSig funcSigs name)
        sidenames <- case zipWith byReference (funcSig ^. funcParamTypes) vecArgs
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
        vecRange <- vectorizeAtom (forCycle ^. forRange)
        let np f = f (forCycle ^. forName) Vec.Immutable
        (changed, vecStatement)
            <- local (np M.insert)
             $ vectorizeStatement funcSigs (forCycle ^. forStatement)
        argIndices <- closedIndices changed
        let vecLambda = Vec.Lambda [patTuple argIndices, np Vec.PAccess] vecStatement
        let vecForCycle = Vec.ForCycle vecLambda (expTuple argIndices) vecRange
        return (changed, vecForCycle)
    IfStatement ifb -> over _2 Vec.MultiIfStatement <$> do
        vecCond <- vectorizeAtom (ifb ^. ifCond)
        (changedThen, vecBodyThenGen) <- vectorizeScope funcSigs (Scope M.empty (ifb ^. ifThen))
        (changedElse, vecBodyElseGen) <- vectorizeScope funcSigs (Scope M.empty (ifb ^. ifElse))
        let changed = nub $ changedThen ++ changedElse
        let accessChanged = map Access changed
        vecBodyThen <- lift $ vecBodyThenGen accessChanged
        vecBodyElse <- lift $ vecBodyElseGen accessChanged
        let vecMultiIf = Vec.MultiIf
                [ (vecCond, Vec.BodyStatement vecBodyThen)
                , (Vec.Primary (LitBoolean True), Vec.BodyStatement vecBodyElse)
                ]
        return $ (changed, vecMultiIf :: Vec.MultiIf Vec.Statement)

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
lookupFuncSig _ (NameOp _) = return $ FuncSig (Name "") [] (TypeUnit)
lookupFuncSig funcSigs name
    = find (\funcSig -> view funcName funcSig == name) funcSigs
    & maybe (throwError $ NoFunction name) return

registerIndexUpdate :: Name -> S E (Name, Vec.Index)
registerIndexUpdate name = do
    index <- readerToState (lookupIndex name >>= indexUpdate)
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
