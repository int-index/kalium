{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..)) where

import Data.List
import Data.Monoid
import Data.Traversable
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Vector.Program as Vec

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

indexTag :: Vec.IndexTag -> Name -> Vec.Name
indexTag Vec.GlobalTag (NameOp op) = NameOp op
indexTag tag (Name1 ns _) = Name1 ns tag
indexTag _ _ = error "indexTag: impossible"

retag :: Name -> Vec.Name
retag = indexTag Vec.GlobalTag

type Indices = M.Map Name Index
type Types   = M.Map Name Type

vectorize :: E e m => Program Type Pattern Atom -> m Vec.Program
vectorize program = do
    vecFuncs <- traverse vectorizeFunc (program ^. programFuncs & M.toList)
    return $ Vec.Program vecFuncs

vectorizeFunc :: E e m => (Name, Func Type Pattern Atom) -> m Vec.Func
vectorizeFunc (name, func) = do
    let params = func ^. funcScope . scopeVars
    let r = vectorizeBody (func ^. funcScope . scopeElem)
    let zeroIndex = Index 0
    (_, vecBody)
            <- runReaderT r
            $ initIndices zeroIndex (scoping params)
    let FuncSig tyResult tyArgs = funcSig func
        vecFuncType = foldr1 TypeFunction (tyArgs `snoc` TypeTaint tyResult)
    let mkParam (name, ty) = smartPAccess name ty zeroIndex
        vecFuncLambda = Vec.lambda (map mkParam params) vecBody
    return $ Vec.Func vecFuncType (retag name) vecFuncLambda

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.LitUnit
mkExpTuple exps = foldr1 (Vec.AppOp2 Vec.OpPair) exps

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
mkAccess name = smartAccess name <$> lookupIndex name

expTuple :: V e t m => [Name] -> t m Vec.Expression
expTuple names = mkExpTuple <$> traverse mkAccess names

patTuple :: V e t m => [Name] -> t m Vec.Pattern
patTuple names = mkPatTuple <$> traverse mkPAccess names

vectorizeBody :: (V e t m, Scoping v) => Scope v Body Pattern Atom -> t m ([Name], Vec.Expression)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    (changed, vecBodyGen) <- vectorizeScope (Scope vars statement)
    vecBody <- lift $ vecBodyGen [result]
    return (changed, vecBody)

updateLocalize names action = do
    updated <- for names $ \name -> do
        index <- lookupIndex name >>= \case
            Index n -> return (Index $ succ n)
            Uninitialized -> return (Index 0)
            Immutable -> throwError (errorUpdateImmutable name)
        return (name, index)
    local ((mempty, M.fromList updated) <>) action

vectorizeScope :: (V e t m, Scoping v) => Scope v Statement Pattern Atom -> t m ([Name], [Atom] -> m Vec.Expression)
vectorizeScope scope = do
    let vars = scope ^. scopeVars . to scoping
    local (initIndices Uninitialized vars <>) $ do
        (changed, vecStatement) <- vectorizeStatement (scope ^. scopeElem)
        updateLocalize changed $ do
            indices <- ask
            let vecBodyGen results = flip runReaderT indices $ do
                  pat <- patTuple changed
                  vecResult <- Vec.Taint . mkExpTuple <$> traverse vectorizeAtom results
                  return $ Vec.Follow pat vecStatement vecResult
            let changedNonlocal = filter (`M.notMember` vars) changed
            return (changedNonlocal, vecBodyGen)

vectorizeStatement :: V e t m => Statement Pattern Atom -> t m ([Name], Vec.Expression)
vectorizeStatement = \case
    Pass -> return ([], Vec.Taint Vec.LitUnit)
    Follow st1 st2 -> do
        (changed1, vecStatement1) <- vectorizeStatement st1
        updateLocalize changed1 $ do
            vecPat1 <- patTuple changed1
            (changed2, vecStatement2) <- vectorizeStatement st2
            updateLocalize changed2 $ do
                vecPat2 <- patTuple changed2
                let changed = nub $ changed1 ++ changed2
                results <- Vec.Taint . mkExpTuple <$> traverse vectorizeAtom (map Access changed)
                let vecBody = Vec.Follow vecPat1 vecStatement1
                            $ Vec.Follow vecPat2 vecStatement2
                            $ results
                return (changed, vecBody)
    ScopeStatement scope -> do
        (changed, vecBodyGen) <- vectorizeScope scope
        vecBody <- lift $ vecBodyGen (map Access changed)
        return (changed, vecBody)
    Execute (Exec pat name _tyArgs args) -> do
        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp OpReadLn  -> True
              NameOp OpGetLn      -> True
              NameOp OpPrintLn    -> True
              NameOp OpPutLn -> True
              Name1 _ _ -> True
              _ -> False
        vecArgs <- traverse vectorizeAtom args
        let patFlatten = \case
              PWildCard -> []
              PUnit -> []
              PAccess name -> [name]
              PTuple p1 p2 -> patFlatten p1 ++ patFlatten p2
        let changed = patFlatten pat
        updateLocalize changed $ do
            vecPattern <- vectorizePattern pat
            results <- Vec.Taint . mkExpTuple <$> traverse vectorizeAtom (map Access changed)
            let vecTaint = if impure then id else Vec.Taint
                vecCall = foldl1 Vec.Beta $ Vec.Access (retag name):vecArgs
                vecExecute = vecTaint vecCall
                vecBody = Vec.Follow vecPattern vecExecute results
            return (changed, vecBody)
    ForStatement forCycle -> do
        vecRange <- vectorizeAtom (forCycle ^. forRange)
        let iter_name  = forCycle ^. forName
            iter_index = Immutable
        iter_type <- lookupType iter_name
        local ( (M.singleton iter_name iter_type
               , M.singleton iter_name iter_index) <>) $ do
            (changed, vecStatement) <- vectorizeStatement (forCycle ^. forStatement)
            argExp <- expTuple changed
            argPat <- patTuple changed
            iterPat <- mkPAccess iter_name
            let vecLambda = Vec.lambda [argPat, iterPat] vecStatement
            let vecFor = Vec.AppOp3 Vec.OpFoldTainted vecLambda argExp vecRange
            return (changed, vecFor)
    IfStatement ifb -> do
        vecCond <- vectorizeAtom (ifb ^. ifCond)
        let noscope = Scope (M.empty :: M.Map Name Type)
        (changedThen, vecBodyThenGen) <- vectorizeScope (ifb ^. ifThen & noscope)
        (changedElse, vecBodyElseGen) <- vectorizeScope (ifb ^. ifElse & noscope)
        let changed = nub $ changedThen ++ changedElse
        let accessChanged = map Access changed
        vecBodyThen <- lift $ vecBodyThenGen accessChanged
        vecBodyElse <- lift $ vecBodyElseGen accessChanged
        let vecIf = Vec.AppOp3 Vec.OpIf vecBodyElse vecBodyThen vecCond
        return (changed, vecIf)

vectorizeAtom :: V e t m => Atom -> t m Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary a)
    Access name -> mkAccess name

vectorizePattern :: V e t m => Pattern -> t m Vec.Pattern
vectorizePattern = \case
    PUnit -> return Vec.PUnit
    PWildCard -> return Vec.PWildCard
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

initIndices :: Index -> M.Map Name Type -> (Types, Indices)
initIndices n types = (types, M.map (const n) types)
