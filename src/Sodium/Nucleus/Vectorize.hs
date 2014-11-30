{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..)) where

import Data.List
import Data.Monoid
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
    errorNoAccess :: Name -> [Name] -> e
    errorUpdateImmutable :: Name -> e

type E e m = (Applicative m, Error e, MonadError e m)
type V e t m = (MonadTrans t, MonadReader (Types, Indices) (t m), E e m, E e (t m))

data Index
    = Index Integer
    | Immutable
    | Uninitialized
    deriving (Eq, Ord, Show)

type Indices = M.Map Name Index
type Types   = M.Map Name Type

vectorize :: E e m => Program Type Pattern Atom -> m Vec.Program
vectorize program = do
    vecFuncs <- mapM vectorizeFunc (program ^. programFuncs & M.toList)
    return $ Vec.Program vecFuncs

vectorizeFunc :: E e m => (Name, Func Type Pattern Atom) -> m Vec.Func
vectorizeFunc (name, func) = do
    let params = func ^. funcScope . scopeVars
    let r = vectorizeBody (func ^. funcScope . scopeElem)
    let zeroIndex = Index 0
    (_, vecBody)
            <- runReaderT r
            $ initIndices zeroIndex (scoping params)
    let vecFuncSig = Vec.FuncSig (retag name)
          (func & funcSig & funcSigParamTypes)
          (func & funcSig & funcSigType)
    let mkParam (name, ty) = smartPAccess name ty zeroIndex
        vecFuncLambda = Vec.Lambda (map mkParam params) (Vec.BodyStatement vecBody)
    return $ Vec.Func vecFuncSig (Vec.LambdaStatement vecFuncLambda)

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.Primary (Lit STypeUnit ())
mkExpTuple pats = foldr1 (Vec.OpAccess OpPair `Vec.Call2`) pats

smartPAccess name ty = \case
    Uninitialized -> Vec.PWildCard
    Index index   -> Vec.PAccess (Vec.IndexTag index `indexTag` name) ty
    Immutable     -> Vec.PAccess (Vec.ImmutableTag   `indexTag` name) ty

smartAccess name = \case
    Uninitialized -> Vec.Access (NameOp OpUndefined)
    Index index   -> Vec.Access (Vec.IndexTag index `indexTag` name)
    Immutable     -> Vec.Access (Vec.ImmutableTag   `indexTag` name)

mkPAccess :: V e t m => Name -> t m Vec.Pattern
mkPAccess name = smartPAccess name <$> lookupType name <*> lookupIndex name

mkAccess :: V e t m => Name -> t m Vec.Expression
mkAccess name = do
    index <- lookupIndex name
    return (smartAccess name index)

expTuple :: V e t m => [Name] -> t m Vec.Expression
expTuple names = mkExpTuple <$> mapM mkAccess names

patTuple :: V e t m => [Name] -> t m Vec.Pattern
patTuple names = mkPatTuple <$> mapM mkPAccess names

vectorizeBody :: (V e t m, Scoping v) => Scope v Body Pattern Atom -> t m ([Name], Vec.Body Vec.Statement)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    (changed, vecBodyGen) <- vectorizeScope (Scope vars statement)
    vecBody <- lift $ vecBodyGen [result]
    return (changed, vecBody)

namingIndexUpdates :: V e t m => [Name] -> t m (Pairs Name Index)
namingIndexUpdates = mapM (naming indexUpdate)

vectorizeScope :: (V e t m, Scoping v) => Scope v Statement Pattern Atom -> t m ([Name], [Atom] -> m (Vec.Body Vec.Statement))
vectorizeScope scope = do
    let vars = scope ^. scopeVars . to scoping
    local (initIndices Uninitialized vars <>) $ do
        (changed, vecStatement) <- vectorizeStatement (scope ^. scopeElem)
        boundIndices <- namingIndexUpdates changed
        local ((mempty, M.fromList boundIndices) <>) $ do
            pat <- patTuple changed
            indices <- ask
            let vecBodyGen results
                    = Vec.Body
                        [Vec.Bind pat vecStatement]
                    <$> runReaderT (Vec.Assign . mkExpTuple <$> mapM vectorizeAtom results) indices
            let changedNonlocal = filter (`M.notMember` vars) changed
            return (changedNonlocal, vecBodyGen)

vectorizeStatement :: V e t m => Statement Pattern Atom -> t m ([Name], Vec.Statement)
vectorizeStatement = \case
    Pass -> return ([], Vec.Assign $ Vec.Primary (Lit STypeUnit ()))
    Follow st1 st2 -> do
        (changed1, vecStatement1) <- vectorizeStatement st1
        boundIndices1 <- namingIndexUpdates changed1
        local ((mempty, M.fromList boundIndices1) <>) $ do
            vecBind1 <- Vec.Bind <$> patTuple changed1 <*> pure vecStatement1
            (changed2, vecStatement2) <- vectorizeStatement st2
            boundIndices2 <- namingIndexUpdates changed2
            local ((mempty, M.fromList boundIndices2) <>) $ do
                vecBind2 <- Vec.Bind <$> patTuple changed2 <*> pure vecStatement2
                let changed = nub $ changed1 ++ changed2
                results <- mkExpTuple <$> mapM vectorizeAtom (map Access changed)
                let vecBody  = Vec.Body [vecBind1, vecBind2] (Vec.Assign results)
                return (changed, Vec.BodyStatement vecBody)
    ScopeStatement scope -> do
        (changed, vecBodyGen) <- vectorizeScope scope
        vecBody <- lift $ vecBodyGen (map Access changed)
        return (changed, Vec.BodyStatement vecBody)
    Execute (Exec pat name args) -> do
        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp OpReadLn  -> True
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
        local ((mempty, M.fromList boundIndices) <>) $ do
            vecPattern <- vectorizePattern pat
            results <- mkExpTuple <$> mapM vectorizeAtom (map Access changed)
            let vecExecute
                  | impure    = Vec.Execute (Vec.Access (retag name)) vecArgs
                  | otherwise = Vec.Assign (foldl1 Vec.Call $ Vec.Access (retag name):vecArgs)
                vecBind = Vec.Bind vecPattern vecExecute
                vecBody = Vec.Body [vecBind] (Vec.Assign results)
            return (changed, Vec.BodyStatement vecBody)
    ForStatement forCycle -> do
        vecRange <- vectorizeAtom (forCycle ^. forRange)
        let iter_name  = forCycle ^. forName
            iter_index = Immutable
            iter_type  = TypeUnit -- TODO: the actual type
        local ( (M.singleton iter_name iter_type
               , M.singleton iter_name iter_index) <>) $ do
            (changed, vecStatement) <- vectorizeStatement (forCycle ^. forStatement)
            argExp <- expTuple changed
            argPat <- patTuple changed
            iterPat <- mkPAccess iter_name
            let vecLambda = Vec.Lambda [argPat, iterPat] vecStatement
            let vecForCycle = Vec.ForCycle (Vec.LambdaStatement vecLambda) argExp vecRange
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
    PAccess name -> mkPAccess name
    PTuple p1 p2 -> Vec.PTuple <$> vectorizePattern p1 <*> vectorizePattern p2

lookupIndex :: V e t m => Name -> t m Index
lookupIndex = lookupX snd

lookupType :: V e t m => Name -> t m Type
lookupType = lookupX fst

lookupX :: V e t m => ((Types, Indices) -> M.Map Name x) -> Name -> t m x
lookupX f name = do
    xs <- asks f
    M.lookup name xs
       & maybe (throwError $ errorNoAccess name (M.keys xs)) return

indexUpdate :: V e t m => Name -> t m Index
indexUpdate name = lookupIndex name >>= \case
    Index n -> return (Index $ succ n)
    Uninitialized -> return (Index 0)
    Immutable -> throwError (errorUpdateImmutable name)

naming op = \name -> (,) name <$> op name

initIndices :: Index -> M.Map Name Type -> (Types, Indices)
initIndices n types = (types, M.map (const n) types)
