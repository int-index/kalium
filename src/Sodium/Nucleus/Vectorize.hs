{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..)) where

import Data.List
import Data.Monoid
import Data.Traversable
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Supply
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
        , vsNames   :: M.Map (Name, Index) (Maybe Vec.Name)
        } deriving (Eq)

                |]

instance Monoid VectorizeScope where
    mempty = VectorizeScope mempty mempty mempty
    vs1 `mappend` vs2 = VectorizeScope
        (vs1 ^. vsTypes   <> vs2 ^. vsTypes)
        (vs1 ^. vsIndices <> vs2 ^. vsIndices)
        (vs1 ^. vsNames   <> vs2 ^. vsNames)

class Error e where
    errorNoAccess :: Name -> [Name] -> e
    errorUpdateImmutable :: Name -> e
    errorInsane :: String -> e

type E e m = (Applicative m, Error e, MonadError e m, MonadSupply Integer m)
type V e m = (MonadReader VectorizeScope m, E e m)

vectorize :: E e m => Program Type Pattern Atom -> m Vec.Program
vectorize program = do
    (mconcat -> names') <- for (program ^. programFuncs . to M.keys)
            $ \name -> M.singleton (name, Immutable) <$> (Just . Vec.NameGen <$> supply)
    flip runReaderT (VectorizeScope mempty mempty names') $ do
        vecFuncs <- traverse (uncurry vectorizeFunc) (program ^. programFuncs & M.toList)
        return $ Vec.Program vecFuncs

vectorizeFunc :: V e m => Name -> Func Type Pattern Atom -> m Vec.Func
vectorizeFunc name func = do
    let zeroIndex = Index 0
    let params = func ^. funcScope . scopeVars
    name' <- getFuncName name
    initialScope <- initIndices zeroIndex (scoping params)
    local (initialScope <>) $ do
        (_, vecBody) <- vectorizeBody (func ^. funcScope . scopeElem)
        let FuncSig tyResult tyArgs = funcSig func
            vecFuncType = foldr1 TypeFunction (tyArgs `snoc` TypeTaint tyResult)
        vecParams <- traverse mkPAccess (map fst params)
        let vecFuncLambda = Vec.lambda vecParams vecBody
        return $ Vec.Func vecFuncType name' vecFuncLambda

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.LitUnit
mkExpTuple exps = foldr1 (Vec.AppOp2 Vec.OpPair) exps

tryPAccess _  Nothing     = Vec.PWildCard
tryPAccess ty (Just name) = Vec.PAccess name ty

tryAccess Nothing     = Vec.Access (Vec.NameSpecial Vec.OpUndefined)
tryAccess (Just name) = Vec.Access name

mkPAccess :: V e m => Name -> m Vec.Pattern
mkPAccess name = do
    index <- lookupIndex name
    ty    <- lookupType  name
    tryPAccess ty <$> lookupName name index

mkAccess :: V e m => Name -> m Vec.Expression
mkAccess name = do
    index <- lookupIndex name
    tryAccess <$> lookupName name index

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
    (unzip -> (names', updated)) <- for names $ \name -> do
        (index, name') <- lookupIndex name >>= \case
            Index n -> do
                name' <- Just . Vec.NameGen <$> supply
                return (Index $ succ n, name')
            Uninitialized -> do
                name' <- Just . Vec.NameGen <$> supply
                return (Index 0, name')
            Immutable -> throwError (errorUpdateImmutable name)
        return (((name, index), name'), (name, index))
    local ( (vsIndices %~ mappend (M.fromList updated))
          . (vsNames   %~ mappend (M.fromList names'))
          ) action

vectorizeResults :: V e m => [Atom] -> m Vec.Expression
vectorizeResults results = Vec.Taint . mkExpTuple <$> traverse vectorizeAtom results

vectorizeScope :: (V e m, Scoping v) => Scope v Statement Pattern Atom -> ([Name] -> [Atom]) -> m ([Name], Vec.Expression)
vectorizeScope (Scope (scoping -> vars) statement) resulting = do
    localScope <- initIndices Uninitialized vars
    local (localScope <>) $ do
        (changed, vecStatement) <- vectorizeStatement statement
        updateLocalize changed $ do
            let changedNonlocal = filter (`M.notMember` vars) changed
            pat <- patTuple changed
            vecResult <- vectorizeResults (resulting changedNonlocal)
            let obj = Vec.Follow pat vecStatement vecResult
            return (changedNonlocal, obj)

vectorizeAgainst changed (map Access -> results)
    = updateLocalize changed
    $ Vec.Lambda <$> patTuple changed <*> vectorizeResults results

getFuncName name = case name of
    NameSpecial op -> return . Vec.NameSpecial $ case op of
     OpAdd -> Vec.OpAdd
     OpSubtract -> Vec.OpSubtract
     OpMultiply -> Vec.OpMultiply
     OpDivide -> Vec.OpDivide
     OpDiv -> Vec.OpDiv
     OpMod -> Vec.OpMod
     OpLess -> Vec.OpLess
     OpMore -> Vec.OpMore
     OpEquals -> Vec.OpEquals
     OpAnd -> Vec.OpAnd
     OpOr -> Vec.OpOr
     OpNot -> Vec.OpNot
     OpXor -> Vec.OpXor
     OpTrue -> Vec.OpTrue
     OpFalse -> Vec.OpFalse
     OpRange -> Vec.OpRange
     OpElem -> Vec.OpElem
     OpShow -> Vec.OpShow
     OpNegate -> Vec.OpNegate
     OpPrintLn -> Vec.OpPrintLn
     OpReadLn -> Vec.OpReadLn
     OpPutLn -> Vec.OpPutLn
     OpGetLn -> Vec.OpGetLn
     OpId -> Vec.OpId
     OpUnit -> Vec.OpUnit
     OpPair -> Vec.OpPair
     OpFst -> Vec.OpFst
     OpSnd -> Vec.OpSnd
     OpNil -> Vec.OpNil
     OpCons -> Vec.OpCons
     OpSingleton -> Vec.OpSingleton
     OpConcat -> Vec.OpConcat
     OpIntToDouble -> Vec.OpIntToDouble
     OpMain -> Vec.OpMain
    NameGen _ -> do
        Just name' <- lookupName name Immutable
        return name'

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
              NameSpecial OpReadLn  -> True
              NameSpecial OpGetLn      -> True
              NameSpecial OpPrintLn    -> True
              NameSpecial OpPutLn -> True
              NameGen _ -> True
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
            name' <- getFuncName name
            let vecTaint = if impure then id else Vec.Taint
                vecCall = foldl1 Vec.Beta (Vec.Access name' : vecArgs)
                vecExecute = vecTaint vecCall
                vecBody = Vec.Follow vecPattern vecExecute results
            return (changed, vecBody)
    ForStatement forCycle -> do
        vecRange <- vectorizeAtom (forCycle ^. forRange)
        let iter_name  = forCycle ^. forName
            iter_index = Immutable
        iter_type <- lookupType iter_name
        iterScope <- initIndices iter_index (M.singleton iter_name iter_type)
        local (iterScope <>) $ do
            -- dry run to see what changes
            (changed, _) <- vectorizeStatement (forCycle ^. forStatement)
            argExp <- expTuple changed
            updateLocalize changed $ do
                (changed', vecStatement) <- vectorizeStatement (forCycle ^. forStatement)
                when (changed /= changed') $
                    throwError (errorInsane "changed == changed'")
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
        vecBodyThen <- Vec.Bind vecStatementThen <$> vectorizeAgainst changedThen changed
        vecBodyElse <- Vec.Bind vecStatementElse <$> vectorizeAgainst changedElse changed
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

lookupName :: V e m => Name -> Index -> m (Maybe Vec.Name)
lookupName name index =
    views vsNames (M.lookup (name, index))
       >>= maybe (throwError $ errorNoAccess name []) return

lookupX :: V e m => (VectorizeScope -> M.Map Name x) -> Name -> m x
lookupX f name = do
    xs <- asks f
    M.lookup name xs
       & maybe (throwError $ errorNoAccess name (M.keys xs)) return

initIndices :: E e m => Index -> M.Map Name Type -> m VectorizeScope
initIndices n types = do
    scopes <- for (M.toList types) $ \(name, ty) -> do
        name' <- case n of
            Uninitialized -> return Nothing
            _ -> Just . Vec.NameGen <$> supply
        return $ VectorizeScope
            (M.singleton name ty)
            (M.singleton name n)
            (M.singleton (name, n) name')
    return (mconcat scopes)
