{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..), degroup) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Program.Vector as Vec

data Error
    = NoAccess Name Vec.Indices
    | NoFunction Name
    | UpdateImmutable Name
    | NoReference
    deriving (Show)

type E m = (Applicative m, MonadError Error m)
type V t m = (MonadTrans t, MonadReader Vec.Indices (t m), E m, E (t m))

vectorize :: E m => Program Atom -> m Vec.Program
vectorize program = do
    let funcSigs = program ^. programFuncs & M.map funcSig
    vecFuncs <- mapM (vectorizeFunc funcSigs) (program ^. programFuncs & M.toList)
    return $ Vec.Program vecFuncs

references :: [(Name, ByType)] -> ([Name], [Type])
references params = unzip $ do
    (name, (by, ty)) <- params
    guard (by == ByReference)
    return (name, ty)

vectorizeFunc :: E m => M.Map Name FuncSig -> (Name, Func Atom) -> m Vec.Func
vectorizeFunc funcSigs (name, func) = do
    let params = func ^. funcScope . scopeVars
    let (refnames, reftypes) = references params
    let r = vectorizeBody funcSigs (func ^. funcScope . scopeElem) (map Access refnames)
    (_, vecBody)
            <- runReaderT r
            $ initIndices (Vec.Index 0) (scoping params)
    let vecFuncSig = Vec.FuncSig name
          (func & funcSig & funcSigParamTypes & map snd)
          (func & funcSig & funcSigType)
          reftypes
    return $ Vec.Func vecFuncSig (params & map fst) (Vec.BodyStatement vecBody)

patTuple = Vec.PTuple . map (uncurry Vec.PAccess)
expTuple = Vec.Tuple  . map (uncurry Vec.Access)

vectorizeBody :: (V t m, Scoping v) => M.Map Name FuncSig -> Scope v Body Atom -> [Atom] -> t m ([Name], Vec.Body)
vectorizeBody funcSigs scope results = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    (changed, vecBodyGen) <- vectorizeScope funcSigs (Scope vars statement)
    vecBody <- lift $ vecBodyGen (result:results)
    return (changed, vecBody)


vectorizeScope :: (V t m, Scoping v) => M.Map Name FuncSig -> Scope v Statement Atom -> t m ([Name], [Atom] -> m Vec.Body)
vectorizeScope funcSigs scope = do
    let vars = scope ^. scopeVars . to scoping
    local (initIndices Vec.Uninitialized vars `M.union`) $ do
        (boundIndices, vecBind) <- do
            (changed, vecStatement) <- vectorizeStatement funcSigs (scope ^. scopeElem)
            changed' <- mapM (naming indexUpdate) changed
            return (changed', Vec.Bind (patTuple changed') vecStatement)
        let changed = filter (`M.notMember` vars) (map fst boundIndices)
        indices' <- asks (M.fromList boundIndices `M.union`)
        let vecBodyGen results
                = Vec.Body vars [vecBind]
                <$> runReaderT (Vec.Tuple <$> mapM vectorizeAtom results) indices'
        return (changed, vecBodyGen)

degroup funcSigs statements act = do
    indices <- ask
    flip evalStateT indices $ do
       vecBinds <- forM statements $ \statement -> do
          (changed, vecStatement) <- readerToState (vectorizeStatement funcSigs statement)
          changed' <- forM changed $ naming $ \name -> do
                index <- readerToState (indexUpdate name)
                modify $ M.insert name index
                return index
          return $ Vec.Bind (patTuple changed') vecStatement
       readerToState (act indices vecBinds)

diff :: (Ord k, Eq v) => M.Map k v -> M.Map k v -> [k]
diff m1 m2 = M.keys $ M.filter id $ M.intersectionWith (/=) m1 m2

vectorizeStatement :: V t m => M.Map Name FuncSig -> Statement Atom -> t m ([Name], Vec.Statement)
vectorizeStatement funcSigs = \case
    Group statements -> do
        degroup funcSigs statements
            $ \indices vecBinds -> do
                changed <- asks (diff indices)
                results <- Vec.Tuple <$> mapM vectorizeAtom (map Access changed)
                let vecBody = Vec.Body M.empty vecBinds results
                return (changed, Vec.BodyStatement vecBody)
    ScopeStatement scope -> do
        (changed, vecBodyGen) <- vectorizeScope funcSigs scope
        vecBody <- lift $ vecBodyGen (map Access changed)
        return (changed, Vec.BodyStatement vecBody)
    Execute (Exec mres name args) -> do
        vecArgs <- mapM vectorizeAtom args
        let byReference (ByReference, _) = \case
              Vec.Access name _ -> Just [name]
              _                 -> Nothing
            byReference (ByValue, _) = const (Just [])
        funcSig <- lookupFuncSig funcSigs name
        sidenames <- case zipWith byReference (funcSigParamTypes funcSig) vecArgs
                          & sequence
                     of Nothing -> throwError NoReference
                        Just ns -> return (concat ns)
        let resnames = maybe empty pure mres
        -- BUG: if a function is called without a return value (mres = Nothing)
        -- then its value isn't bound anywhere, resulting in a pattern tuple
        -- with incorrect amount of variables

        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp (OpReadLn _) -> True
              NameOp OpPrintLn    -> True
              _ -> False
        let vecExecute
              | impure    = Vec.Execute name vecArgs
              | otherwise = Vec.Assign (Vec.Call name vecArgs)
        return $ (resnames ++ sidenames, vecExecute)
    ForStatement forCycle -> do
        vecRange <- vectorizeAtom (forCycle ^. forRange)
        let np f = f (forCycle ^. forName) Vec.Immutable
        (changed, vecStatement)
            <- local (np M.insert)
             $ vectorizeStatement funcSigs (forCycle ^. forStatement)
        argIndices <- mapM (naming lookupIndex) changed
        let vecLambda = Vec.Lambda [patTuple argIndices, np Vec.PAccess] vecStatement
        let vecForCycle = Vec.ForCycle vecLambda (expTuple argIndices) vecRange
        return (changed, Vec.ForStatement vecForCycle)
    IfStatement ifb -> do
        vecCond <- vectorizeAtom (ifb ^. ifCond)
        let noscope = Scope (M.empty :: M.Map Name Type)
        (changedThen, vecBodyThenGen) <- vectorizeScope funcSigs (ifb ^. ifThen & noscope)
        (changedElse, vecBodyElseGen) <- vectorizeScope funcSigs (ifb ^. ifElse & noscope)
        let changed = nub $ changedThen ++ changedElse
        let accessChanged = map Access changed
        vecBodyThen <- lift $ vecBodyThenGen accessChanged
        vecBodyElse <- lift $ vecBodyElseGen accessChanged
        let vecMultiIf = Vec.MultiIf
                [ (vecCond, Vec.BodyStatement vecBodyThen)
                , (Vec.Primary (LitBoolean' True), Vec.BodyStatement vecBodyElse)
                ]
        return $ (changed, Vec.MultiIfStatement vecMultiIf)

vectorizeAtom :: V t m => Atom -> t m Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary (Literal' a))
    Access name -> Vec.Access name <$> lookupIndex name

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex :: V t m => Name -> t m Vec.Index
lookupIndex name = do
    indices <- ask
    M.lookup name indices
       & maybe (throwError $ NoAccess name indices) return

lookupFuncSig :: E m => M.Map Name FuncSig -> Name -> m FuncSig
-- TODO: real signatures for builtin operators
lookupFuncSig _ (NameOp _) = return $ FuncSig TypeUnit []
lookupFuncSig funcSigs name
    = M.lookup name funcSigs
    & maybe (throwError $ NoFunction name) return

indexUpdate :: V t m => Name -> t m Vec.Index
indexUpdate name = lookupIndex name >>= \case
    Vec.Index n -> return (Vec.Index $ succ n)
    Vec.Uninitialized -> return (Vec.Index 0)
    Vec.Immutable -> throwError (UpdateImmutable name)

naming op = \name -> (,) name <$> op name

initIndices :: Vec.Index -> M.Map Name a -> Vec.Indices
initIndices n = M.map (const n)
