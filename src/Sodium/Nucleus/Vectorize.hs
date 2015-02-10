{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Sodium.Nucleus.Vectorize (vectorize, Error(..)) where

import Sodium.Prelude
import Sodium.Util

import qualified Data.Map as M
import Sodium.Nucleus.Scalar.Program
import qualified Sodium.Nucleus.Scalar.Operator as Op
import qualified Sodium.Nucleus.Vector.Program as Vec

data Index
    = Index Integer
    | Immutable
    | Uninitialized
    deriving (Eq, Ord, Show)

declareLenses [d|

    data VectorizeScope = VectorizeScope
        { vsTypes   :: Map Name Type
        , vsIndices :: Map Name Index
        , vsNames   :: Map (Name, Index) (Maybe Vec.Name)
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

type E e m = (Applicative m, Error e, MonadError e m, MonadNameGen m)
type V e m = (MonadReader VectorizeScope m, E e m)

alias :: E e m => Name -> m Vec.Name
alias (NameGen m) = Vec.NameGen <$> rename m
alias _ = Vec.NameGen <$> mkname Nothing

vectorize :: E e m => Program Type Pattern Atom -> m Vec.Program
vectorize program = do
    (mconcat -> names') <- for (program ^. programFuncs . to M.keys)
        $ \name -> do
            name' <- case name of
                NameSpecial OpMain -> return (Vec.NameSpecial Vec.OpMain)
                _ -> alias name
            return $ M.singleton (name, Immutable) (Just name')
    flip runReaderT (VectorizeScope mempty mempty names') $ do
        vecFuncs <- traverse (uncurry vectorizeFunc) (program ^. programFuncs & M.toList)
        return $ Vec.Program (M.fromList vecFuncs)

vectorizeFunc :: V e m => Name -> Func Type Pattern Atom -> m (Vec.Name, Vec.Func)
vectorizeFunc name func = do
    let zeroIndex = Index 0
    let params = func ^. funcScope . scopeVars
    name' <- fromJust <$> lookupName name Immutable
    initialScope <- initIndices zeroIndex (scoping params)
    local (initialScope <>) $ do
        (_, vecBody) <- vectorizeBody (func ^. funcScope . scopeElem)
        let FuncSig (vectorizeType -> tyResult) (map vectorizeType -> tyArgs) = funcSig func
            vecFuncType = foldr1 Vec.TypeFunction (tyArgs `snoc` Vec.TypeTaint tyResult)
        vecParams <- traverse mkPAccess (map fst params)
        let vecFuncLambda = Vec.lambda vecParams vecBody
        return (name', Vec.Func vecFuncType vecFuncLambda)

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.LitUnit
mkExpTuple exps = foldr1 (Vec.AppOp2 Vec.OpPair) exps

tryPAccess _  Nothing     = Vec.PWildCard
tryPAccess ty (Just name) = Vec.PAccess name ty

-- TODO: default values for other types
tryAccess ty Nothing = case ty of
    Vec.TypeList _ -> Vec.OpAccess Vec.OpNil
    _ -> Vec.OpAccess Vec.OpUndefined
tryAccess _ (Just name) = Vec.Access name

mkPAccess :: V e m => Name -> m Vec.Pattern
mkPAccess name = do
    index <- lookupIndex name
    ty    <- lookupType  name
    tryPAccess (vectorizeType ty) <$> lookupName name index

mkAccess :: V e m => Name -> m Vec.Expression
mkAccess name = do
    index <- lookupIndex name
    ty    <- lookupType  name
    tryAccess (vectorizeType ty) <$> lookupName name index

expTuple :: V e m => [Name] -> m Vec.Expression
expTuple names = mkExpTuple <$> traverse mkAccess names

patTuple :: V e m => [Name] -> m Vec.Pattern
patTuple names = mkPatTuple <$> traverse mkPAccess names

vectorizeBody :: (V e m, Scoping v) => Scope v Body Pattern Atom -> m ([Name], Vec.Expression)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    vectorizeScope (const [result]) (Scope vars statement)

updateLocalize names action = do
    (unzip -> (names', updated)) <- for names $ \name -> do
        index <- lookupIndex name >>= \case
            Index n -> return (Index $ succ n)
            Uninitialized -> return (Index 0)
            Immutable -> throwError (errorUpdateImmutable name)
        name' <- Just <$> alias name
        return (((name, index), name'), (name, index))
    local ( (vsIndices %~ mappend (M.fromList updated))
          . (vsNames   %~ mappend (M.fromList names'))
          ) action

vectorizeResults :: V e m => [Atom] -> m Vec.Expression
vectorizeResults results = Vec.Taint . mkExpTuple <$> traverse vectorizeAtom results

vectorizeScope :: (V e m, Scoping v) => ([Name] -> [Atom])
    -> Scope v Statement Pattern Atom -> m ([Name], Vec.Expression)
vectorizeScope resulting (Scope (scoping -> vars) statement) = do
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

vectorizeLiteral :: Literal -> Vec.Literal
vectorizeLiteral = \case
    LitInteger a -> Vec.LitInteger a
    LitDouble  a -> Vec.LitDouble  a
    LitChar    a -> Vec.LitChar    a

vectorizeType :: Type -> Vec.Type
vectorizeType = \case
    TypeInteger -> Vec.TypeInteger
    TypeDouble -> Vec.TypeDouble
    TypeBoolean -> Vec.TypeBoolean
    TypeChar -> Vec.TypeChar
    TypeUnit -> Vec.TypeUnit
    TypeList ty -> Vec.TypeList (vectorizeType ty)
    TypePair ty1 ty2 -> Vec.TypePair (vectorizeType ty1) (vectorizeType ty2)

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

    ScopeStatement scope -> vectorizeScope (map Access) scope

    Execute (Exec pat name _tyArgs args) -> do
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
            vecCall <- case M.lookup name Op.operators of
                Nothing -> do
                    Just name' <- lookupName name Immutable
                    return (Vec.beta . (Vec.Access name':))
                Just op -> return (Op.vec op)
            let vecBody = Vec.Follow vecPattern (vecCall vecArgs)results
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

vectorizeAtom :: V e m => Kleisli' m Atom Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary (vectorizeLiteral a))
    Access name -> mkAccess name

vectorizePattern :: V e m => Kleisli' m Pattern Vec.Pattern
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

lookupX :: V e m => (VectorizeScope -> Map Name x) -> Name -> m x
lookupX f name = do
    xs <- asks f
    M.lookup name xs
       & maybe (throwError $ errorNoAccess name (M.keys xs)) return

initIndices :: E e m => Index -> Map Name Type -> m VectorizeScope
initIndices n types = do
    scopes <- for (itoList types) $ \(name, ty) -> do
        name' <- case n of
            Uninitialized -> return Nothing
            _ -> Just <$> alias name
        return $ VectorizeScope
            (M.singleton name ty)
            (M.singleton name n)
            (M.singleton (name, n) name')
    return (mconcat scopes)
