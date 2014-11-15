{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..)) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Program.Vector as Vec
import Sodium.Util

data Error
    = NoAccess Name Vec.Indices
    | NoFunction Name
    | UpdateImmutable Name
    | NoReference
    deriving (Show)

type E m = (Applicative m, MonadError Error m)
type V t m = (MonadTrans t, MonadReader Vec.Indices (t m), E m, E (t m))

vectorize :: E m => Program ByType Pattern Atom -> m Vec.Program
vectorize program = do
    let funcSigs = program ^. programFuncs & M.map funcSig
    vecFuncs <- mapM (vectorizeFunc funcSigs) (program ^. programFuncs & M.toList)
    return $ Vec.Program vecFuncs

references :: [(Name, ByType)] -> ([Name], [Type])
references params = unzip $ do
    (name, (by, ty)) <- params
    guard (by == ByReference)
    return (name, ty)

vectorizeFunc :: E m => M.Map Name (FuncSig ByType) -> (Name, Func ByType Pattern Atom) -> m Vec.Func
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

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.Primary (Lit STypeUnit ())
mkExpTuple pats = foldr1 (Vec.CallOp2 OpPair) pats

patTuple = mkPatTuple . map (uncurry Vec.PAccess)
expTuple = mkExpTuple . map (uncurry Vec.Access)

vectorizeBody :: (V t m, Scoping v) => M.Map Name (FuncSig ByType) -> Scope v Body Pattern Atom -> [Atom] -> t m ([Name], Vec.Body)
vectorizeBody funcSigs scope results = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    (changed, vecBodyGen) <- vectorizeScope funcSigs (Scope vars statement)
    vecBody <- lift $ vecBodyGen (result:results)
    return (changed, vecBody)

namingIndexUpdates :: V t m => [Name] -> t m (Pairs Name Vec.Index)
namingIndexUpdates = mapM (naming indexUpdate)

vectorizeScope :: (V t m, Scoping v) => M.Map Name (FuncSig ByType) -> Scope v Statement Pattern Atom -> t m ([Name], [Atom] -> m Vec.Body)
vectorizeScope funcSigs scope = do
    let vars = scope ^. scopeVars . to scoping
    local (initIndices Vec.Uninitialized vars `M.union`) $ do
        (changed, vecStatement) <- vectorizeStatement funcSigs (scope ^. scopeElem)
        boundIndices <- namingIndexUpdates changed
        local (M.fromList boundIndices `M.union`) $ do
            indices <- ask
            let vecBodyGen results
                    = Vec.Body vars [Vec.Bind (patTuple boundIndices) vecStatement]
                    <$> runReaderT (mkExpTuple <$> mapM vectorizeAtom results) indices
            let changedNonlocal = filter (`M.notMember` vars) changed
            return (changedNonlocal, vecBodyGen)

degroup funcSigs st1 st2 = do
    indices <- ask
    (changed1, vecStatement1) <- vectorizeStatement funcSigs st1
    boundIndices1 <- namingIndexUpdates changed1
    local (M.fromList boundIndices1 `M.union`) $ do
        (changed2, vecStatement2) <- vectorizeStatement funcSigs st2
        boundIndices2 <- namingIndexUpdates changed2
        local (M.fromList boundIndices2 `M.union`) $ do
            changed <- asks (diff indices)
            results <- mkExpTuple <$> mapM vectorizeAtom (map Access changed)
            let vecBind1 = Vec.Bind (patTuple boundIndices1) vecStatement1
                vecBind2 = Vec.Bind (patTuple boundIndices2) vecStatement2
                vecBody  = Vec.Body M.empty [vecBind1, vecBind2] results
            return (changed, Vec.BodyStatement vecBody)

diff :: (Ord k, Eq v) => M.Map k v -> M.Map k v -> [k]
diff m1 m2 = M.keys $ M.filter id $ M.intersectionWith (/=) m1 m2

vectorizeStatement :: V t m => M.Map Name (FuncSig ByType) -> Statement Pattern Atom -> t m ([Name], Vec.Statement)
vectorizeStatement funcSigs = \case
    Pass -> return ([], Vec.Assign $ Vec.Primary (Lit STypeUnit ()))
    Follow st1 st2 -> degroup funcSigs st1 st2
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
        let resnames = case mres of
                PUnit -> empty
                PAccess name -> pure name
        -- BUG: if a function is called without a return value (mres = Nothing)
        -- then its value isn't bound anywhere, resulting in a pattern tuple
        -- with incorrect amount of variables

        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp (OpReadLn _) -> True
              NameOp OpGetLn      -> True
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
                , (Vec.Primary (Lit STypeBoolean True), Vec.BodyStatement vecBodyElse)
                ]
        return $ (changed, Vec.MultiIfStatement vecMultiIf)

vectorizeAtom :: V t m => Atom -> t m Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary a)
    Access name -> Vec.Access name <$> lookupIndex name

lookupIndex :: V t m => Name -> t m Vec.Index
lookupIndex name = do
    indices <- ask
    M.lookup name indices
       & maybe (throwError $ NoAccess name indices) return

lookupFuncSig :: E m => M.Map Name (FuncSig ByType) -> Name -> m (FuncSig ByType)
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
