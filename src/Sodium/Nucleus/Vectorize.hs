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
import Sodium.Nucleus.Program.Vector (indexTag, retag)
import qualified Sodium.Nucleus.Program.Vector as Vec
import Sodium.Util

class Error e where
    errorNoAccess :: Name -> Indices -> e
    errorUpdateImmutable :: Name -> e

type E e m = (Applicative m, Error e, MonadError e m)
type V e t m = (MonadTrans t, MonadReader Indices (t m), E e m, E e (t m))

data Index
    = Index Integer
    | Immutable
    | Uninitialized
    deriving (Eq, Ord, Show)

type Indices
    = M.Map Name Index

vectorize :: E e m => Program Type Pattern Atom -> m Vec.Program
vectorize program = do
    vecFuncs <- mapM vectorizeFunc (program ^. programFuncs & M.toList)
    return $ Vec.Program vecFuncs

vectorizeFunc :: E e m => (Name, Func Type Pattern Atom) -> m Vec.Func
vectorizeFunc (name, func) = do
    let params = func ^. funcScope . scopeVars
    let r = vectorizeBody (func ^. funcScope . scopeElem)
    (_, vecBody)
            <- runReaderT r
            $ initIndices (Index 0) (scoping params)
    let vecFuncSig = Vec.FuncSig (retag name)
          (func & funcSig & funcSigParamTypes)
          (func & funcSig & funcSigType)
    let mkParam (name, _) = smartPAccess name (Index 0)
        vecFuncLambda = Vec.Lambda (map mkParam params) (Vec.BodyStatement vecBody)
    return $ Vec.Func vecFuncSig vecFuncLambda

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.Primary (Lit STypeUnit ())
mkExpTuple pats = foldr1 (Vec.OpAccess OpPair `Vec.Call2`) pats

smartPAccess name = \case
    Uninitialized -> Vec.PWildCard
    Index index   -> Vec.PAccess (Vec.IndexTag index `indexTag` name)
    Immutable     -> Vec.PAccess (Vec.ImmutableTag   `indexTag` name)

smartAccess name = \case
    Uninitialized -> Vec.Access (NameOp OpUndefined)
    Index index   -> Vec.Access (Vec.IndexTag index `indexTag` name)
    Immutable     -> Vec.Access (Vec.ImmutableTag   `indexTag` name)

patTuple = mkPatTuple . map (uncurry smartPAccess)
expTuple = mkExpTuple . map (uncurry smartAccess)

vectorizeBody :: (V e t m, Scoping v) => Scope v Body Pattern Atom -> t m ([Name], Vec.Body)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    (changed, vecBodyGen) <- vectorizeScope (Scope vars statement)
    vecBody <- lift $ vecBodyGen [result]
    return (changed, vecBody)

namingIndexUpdates :: V e t m => [Name] -> t m (Pairs Name Index)
namingIndexUpdates = mapM (naming indexUpdate)

theTerribleHackWithVariables :: M.Map Name t -> M.Map (Name1 Vec.IndexTag) t
theTerribleHackWithVariables vars = M.fromList $ do
    (name, t) <- M.toList vars
    tag <- Vec.ImmutableTag : map Vec.IndexTag [0..42]
    return (indexTag tag name, t)


vectorizeScope :: (V e t m, Scoping v) => Scope v Statement Pattern Atom -> t m ([Name], [Atom] -> m Vec.Body)
vectorizeScope scope = do
    let vars = scope ^. scopeVars . to scoping
    local (initIndices Uninitialized vars `M.union`) $ do
        (changed, vecStatement) <- vectorizeStatement (scope ^. scopeElem)
        boundIndices <- namingIndexUpdates changed
        local (M.fromList boundIndices `M.union`) $ do
            indices <- ask
            let vecBodyGen results
                    = Vec.Body (theTerribleHackWithVariables vars)
                        [Vec.Bind (patTuple boundIndices) vecStatement]
                    <$> runReaderT (mkExpTuple <$> mapM vectorizeAtom results) indices
            let changedNonlocal = filter (`M.notMember` vars) changed
            return (changedNonlocal, vecBodyGen)

vectorizeStatement :: V e t m => Statement Pattern Atom -> t m ([Name], Vec.Statement)
vectorizeStatement = \case
    Pass -> return ([], Vec.Assign $ Vec.Primary (Lit STypeUnit ()))
    Follow st1 st2 -> do
        (changed1, vecStatement1) <- vectorizeStatement st1
        boundIndices1 <- namingIndexUpdates changed1
        local (M.fromList boundIndices1 `M.union`) $ do
            (changed2, vecStatement2) <- vectorizeStatement st2
            boundIndices2 <- namingIndexUpdates changed2
            local (M.fromList boundIndices2 `M.union`) $ do
                let changed = nub $ changed1 ++ changed2
                results <- mkExpTuple <$> mapM vectorizeAtom (map Access changed)
                let vecBind1 = Vec.Bind (patTuple boundIndices1) vecStatement1
                    vecBind2 = Vec.Bind (patTuple boundIndices2) vecStatement2
                    vecBody  = Vec.Body M.empty [vecBind1, vecBind2] results
                return (changed, Vec.BodyStatement vecBody)
    ScopeStatement scope -> do
        (changed, vecBodyGen) <- vectorizeScope scope
        vecBody <- lift $ vecBodyGen (map Access changed)
        return (changed, Vec.BodyStatement vecBody)
    Execute (Exec pat name args) -> do
        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp (OpReadLn _) -> True
              NameOp OpGetLn      -> True
              NameOp OpPrintLn    -> True
              _ -> False
        vecArgs <- mapM vectorizeAtom args
        let patFlatten = \case
              PUnit -> []
              PAccess name -> [name]
              PTuple p1 p2 -> patFlatten p1 ++ patFlatten p2
        let changed = patFlatten pat
        boundIndices <- namingIndexUpdates changed
        local (M.fromList boundIndices `M.union`) $ do
            vecPattern <- vectorizePattern pat
            results <- mkExpTuple <$> mapM vectorizeAtom (map Access changed)
            let vecExecute
                  | impure    = Vec.Execute (retag name) vecArgs
                  | otherwise = Vec.Assign (foldl1 Vec.Call $ Vec.Access (retag name):vecArgs)
                vecBind = Vec.Bind vecPattern vecExecute
                vecBody = Vec.Body M.empty [vecBind] results
            return (changed, Vec.BodyStatement vecBody)
    ForStatement forCycle -> do
        vecRange <- vectorizeAtom (forCycle ^. forRange)
        let np f = f (forCycle ^. forName) Immutable
        (changed, vecStatement)
            <- local (np M.insert)
             $ vectorizeStatement (forCycle ^. forStatement)
        argIndices <- mapM (naming lookupIndex) changed
        let vecLambda = Vec.Lambda [patTuple argIndices, np smartPAccess] vecStatement
        let vecForCycle = Vec.ForCycle vecLambda (expTuple argIndices) vecRange
        return (changed, Vec.ForStatement vecForCycle)
    IfStatement ifb -> do
        vecCond <- vectorizeAtom (ifb ^. ifCond)
        let noscope = Scope (M.empty :: M.Map Name Type)
        (changedThen, vecBodyThenGen) <- vectorizeScope (ifb ^. ifThen & noscope)
        (changedElse, vecBodyElseGen) <- vectorizeScope (ifb ^. ifElse & noscope)
        let changed = nub $ changedThen ++ changedElse
        let accessChanged = map Access changed
        vecBodyThen <- lift $ vecBodyThenGen accessChanged
        vecBodyElse <- lift $ vecBodyElseGen accessChanged
        let vecMultiIf = Vec.MultiIf
                [ (vecCond, Vec.BodyStatement vecBodyThen)
                , (Vec.Primary (Lit STypeBoolean True), Vec.BodyStatement vecBodyElse)
                ]
        return $ (changed, Vec.MultiIfStatement vecMultiIf)

vectorizeAtom :: V e t m => Atom -> t m Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary a)
    Access name -> smartAccess name <$> lookupIndex name

vectorizePattern :: V e t m => Pattern -> t m Vec.Pattern
vectorizePattern = \case
    PUnit -> return Vec.PUnit
    PAccess name -> smartPAccess name <$> lookupIndex name
    PTuple p1 p2 -> Vec.PTuple <$> vectorizePattern p1 <*> vectorizePattern p2

lookupIndex :: V e t m => Name -> t m Index
lookupIndex name = do
    indices <- ask
    M.lookup name indices
       & maybe (throwError $ errorNoAccess name indices) return

indexUpdate :: V e t m => Name -> t m Index
indexUpdate name = lookupIndex name >>= \case
    Index n -> return (Index $ succ n)
    Uninitialized -> return (Index 0)
    Immutable -> throwError (errorUpdateImmutable name)

naming op = \name -> (,) name <$> op name

initIndices :: Index -> M.Map Name a -> Indices
initIndices n = M.map (const n)
