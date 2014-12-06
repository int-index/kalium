{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
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

data Index
    = Index Integer
    | Immutable
    | Uninitialized
    deriving (Eq, Ord, Show)

declareLenses [d|

    data VectorizeScope = VectorizeScope
        { vsTypes   :: M.Map Name Type
        , vsIndices :: M.Map Name Index
        } deriving (Eq)

                |]

instance Monoid VectorizeScope where
    mempty = VectorizeScope mempty mempty
    vs1 `mappend` vs2 = VectorizeScope
        (vs1 ^. vsTypes   <> vs2 ^. vsTypes)
        (vs1 ^. vsIndices <> vs2 ^. vsIndices)

class Error e where
    errorNoAccess :: Name -> [Name] -> e
    errorUpdateImmutable :: Name -> e

type E e m = (Applicative m, Error e, MonadError e m)
type V e m = (MonadReader VectorizeScope m, E e m)

indexTag :: Maybe Integer -> Name -> Vec.Name
indexTag Nothing (NameOp op) = NameOp op
indexTag tag (NameGen n ()) = NameGen n tag
indexTag _ _ = error "indexTag: impossible"

retag :: Name -> Vec.Name
retag = indexTag Nothing

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
    Index index   -> Vec.PAccess (Just index  `indexTag` name) ty
    Immutable     -> Vec.PAccess (Nothing     `indexTag` name) ty

smartAccess name = \case
    Uninitialized -> Vec.Access (NameOp OpUndefined)
    Index index   -> Vec.Access (Just index `indexTag` name)
    Immutable     -> Vec.Access (Nothing    `indexTag` name)

mkPAccess :: V e m => Name -> m Vec.Pattern
mkPAccess name = smartPAccess name <$> lookupType name <*> lookupIndex name

mkAccess :: V e m => Name -> m Vec.Expression
mkAccess name = smartAccess name <$> lookupIndex name

expTuple :: V e m => [Name] -> m Vec.Expression
expTuple names = mkExpTuple <$> traverse mkAccess names

patTuple :: V e m => [Name] -> m Vec.Pattern
patTuple names = mkPatTuple <$> traverse mkPAccess names

vectorizeBody :: (V e m, Scoping v) => Scope v Body Pattern Atom -> m ([Name], Vec.Expression)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    vectorizeScope (Scope vars statement) (const [result])

updateLocalize names action = do
    updated <- for names $ \name -> do
        index <- lookupIndex name >>= \case
            Index n -> return (Index $ succ n)
            Uninitialized -> return (Index 0)
            Immutable -> throwError (errorUpdateImmutable name)
        return (name, index)
    local (vsIndices %~ mappend (M.fromList updated)) action

vectorizeResults :: V e m => [Atom] -> m Vec.Expression
vectorizeResults results = Vec.Taint . mkExpTuple <$> traverse vectorizeAtom results

vectorizeScope :: (V e m, Scoping v) => Scope v Statement Pattern Atom -> ([Name] -> [Atom]) -> m ([Name], Vec.Expression)
vectorizeScope (Scope (scoping -> vars) statement) resulting = do
    local (initIndices Uninitialized vars <>) $ do
        (changed, vecStatement) <- vectorizeStatement statement
        updateLocalize changed $ do
            let changedNonlocal = filter (`M.notMember` vars) changed
            pat <- patTuple changed
            vecResult <- vectorizeResults (resulting changedNonlocal)
            let obj = Vec.Follow pat vecStatement vecResult
            return (changedNonlocal, obj)

vectorizeAgainst changed vecStatement results = do
    updateLocalize changed $ do
        pat <- patTuple changed
        vecResult <- vectorizeResults (map Access results)
        return $ Vec.Follow pat vecStatement vecResult

vectorizeStatement :: V e m => Statement Pattern Atom -> m ([Name], Vec.Expression)
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
                results <- vectorizeResults (map Access changed)
                let vecBody = Vec.Follow vecPat1 vecStatement1
                            $ Vec.Follow vecPat2 vecStatement2
                            $ results
                return (changed, vecBody)
    ScopeStatement scope -> vectorizeScope scope (map Access)
    Execute (Exec pat name _tyArgs args) -> do
        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp OpReadLn  -> True
              NameOp OpGetLn      -> True
              NameOp OpPrintLn    -> True
              NameOp OpPutLn -> True
              NameGen _ _ -> True
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
            results <- vectorizeResults (map Access changed)
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
        local (initIndices iter_index (M.singleton iter_name iter_type) <>) $ do
            (changed, vecStatement) <- vectorizeStatement (forCycle ^. forStatement)
            argExp <- expTuple changed
            argPat <- patTuple changed
            iterPat <- mkPAccess iter_name
            let vecLambda = Vec.lambda [argPat, iterPat] vecStatement
            let vecFor = Vec.AppOp3 Vec.OpFoldTainted vecLambda argExp vecRange
            return (changed, vecFor)
    IfStatement ifb -> do
        vecCond <- vectorizeAtom (ifb ^. ifCond)
        (changedThen, vecStatementThen) <- vectorizeStatement (ifb ^. ifThen)
        (changedElse, vecStatementElse) <- vectorizeStatement (ifb ^. ifElse)
        let changed = nub $ changedThen ++ changedElse
        vecBodyThen <- vectorizeAgainst changedThen vecStatementThen changed
        vecBodyElse <- vectorizeAgainst changedElse vecStatementElse changed
        let vecIf = Vec.AppOp3 Vec.OpIf vecBodyElse vecBodyThen vecCond
        return (changed, vecIf)

vectorizeAtom :: V e m => Atom -> m Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary a)
    Access name -> mkAccess name

vectorizePattern :: V e m => Pattern -> m Vec.Pattern
vectorizePattern = \case
    PUnit -> return Vec.PUnit
    PWildCard -> return Vec.PWildCard
    PAccess name -> mkPAccess name
    PTuple p1 p2 -> Vec.PTuple <$> vectorizePattern p1 <*> vectorizePattern p2

lookupIndex :: V e m => Name -> m Index
lookupIndex = lookupX (view vsIndices)

lookupType :: V e m => Name -> m Type
lookupType = lookupX (view vsTypes)

lookupX :: V e m => (VectorizeScope -> M.Map Name x) -> Name -> m x
lookupX f name = do
    xs <- asks f
    M.lookup name xs
       & maybe (throwError $ errorNoAccess name (M.keys xs)) return

initIndices :: Index -> M.Map Name Type -> VectorizeScope
initIndices n types = VectorizeScope types (M.map (const n) types)
