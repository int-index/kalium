{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Kalium.Nucleus.Vectorize (vectorize) where

import Kalium.Prelude
import Kalium.Util

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Rename
import Control.Exception

import Kalium.Error.Insane

import qualified Data.Map as M
import Kalium.Nucleus.Scalar.Program
import qualified Kalium.Nucleus.Scalar.Operator as Op
import qualified Kalium.Nucleus.Vector.Program as Vec

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

instance Exception ErrorNoAccess
data ErrorNoAccess = ErrorNoAccess Name [Name]
    deriving (Show)

instance Exception ErrorUpdateImmutable
data ErrorUpdateImmutable = ErrorUpdateImmutable Name
    deriving (Show)

type E m = (MonadError SomeException m, MonadNameGen m)
type (~>) a b = forall m . (MonadReader VectorizeScope m, E m) => Kleisli' m a b

alias :: E m => Name -> m Vec.Name
alias (NameGen m) = Vec.NameGen <$> rename m
alias _ = Vec.NameGen <$> mkname Nothing

vectorize :: E m => Primitive Program -> m (Vec.Program)
vectorize program = do
    (mconcat -> names') <- for (program ^. programFuncs . to M.keys)
        $ \name -> do
            name' <- case name of
                NameSpecial OpMain -> return (Vec.NameSpecial Vec.OpMain)
                _ -> alias name
            return $ M.singleton (name, Immutable) (Just name')
    flip runReaderT (VectorizeScope mempty mempty names') $ do
        let funcs = M.toList (program ^. programFuncs)
        vecFuncs <- traverse (uncurry vectorizeFunc) funcs
        return $ Vec.Program (M.fromList vecFuncs)

vectorizeFunc :: Name -> Primitive Func ~> (Vec.Name, Vec.Func)
vectorizeFunc name func = do
    let zeroIndex = Index 0
    let params = func ^. funcScope . scopeVars
    name' <- fromJust <$> lookupName name Immutable
    initialScope <- initIndices zeroIndex (scoping params)
    local (initialScope <>) $ do
        (_, vecBody) <- vectorizeBody (func ^. funcScope . scopeElem)
        let FuncSig
                (vectorizeType -> tyResult)
                (map vectorizeType -> tyArgs)
              = funcSig func
            vecFuncType = Vec.tyfun tyArgs (Vec.TypeApp1 Vec.TypeTaint tyResult)
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
    Vec.TypeApp1 Vec.TypeList _ -> Vec.OpAccess Vec.OpNil
    _ -> Vec.OpAccess Vec.OpUndefined
tryAccess _ (Just name) = Vec.Access name

mkPAccess :: Name ~> Vec.Pattern
mkPAccess name = do
    index <- lookupIndex name
    ty    <- lookupType  name
    tryPAccess (vectorizeType ty) <$> lookupName name index

mkAccess :: Name ~> Vec.Expression
mkAccess name = do
    index <- lookupIndex name
    ty    <- lookupType  name
    tryAccess (vectorizeType ty) <$> lookupName name index

expTuple :: [Name] ~> Vec.Expression
expTuple names = mkExpTuple <$> traverse mkAccess names

patTuple :: [Name] ~> Vec.Pattern
patTuple names = mkPatTuple <$> traverse mkPAccess names

vectorizeBody :: Scoping v => Primitive (Scope v Body) ~> ([Name], Vec.Expression)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    vectorizeScope (const [result]) (Scope vars statement)

updateLocalize :: (E m, MonadReader VectorizeScope m) => [Name] -> m a -> m a
updateLocalize names action = do
    (unzip -> (names', updated)) <- for names $ \name -> do
        index <- lookupIndex name >>= \case
            Index n -> return (Index $ succ n)
            Uninitialized -> return (Index 0)
            Immutable -> (throwError.SomeException) (ErrorUpdateImmutable name)
        name' <- Just <$> alias name
        return (((name, index), name'), (name, index))
    local ( (vsIndices %~ mappend (M.fromList updated))
          . (vsNames   %~ mappend (M.fromList names'))
          ) action

vectorizeResults :: [Atom] ~> Vec.Expression
vectorizeResults results
     =  Vec.Taint . mkExpTuple
    <$> traverse vectorizeAtom results

vectorizeScope
    :: Scoping v
    => ([Name] -> [Atom])
    -> Primitive (Scope v Statement)
    ~> ([Name], Vec.Expression)
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
    TypeList -> Vec.TypeList
    TypePair -> Vec.TypePair
    TypeBeta ty1 ty2 -> Vec.TypeBeta
        (vectorizeType ty1) (vectorizeType ty2)
    _ -> error "vectorizeType: unknown type"

vectorizeStatement :: Primitive Statement ~> ([Name], Vec.Expression)
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
                (changed', vecStatement)
                    <- vectorizeStatement (forCycle ^. forStatement)
                when (changed /= changed') $
                    (throwError.SomeException) (ErrorInsane "changed == changed'")
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
        vecBodyThen
            <-  Vec.Bind vecStatementThen
            <$> vectorizeAgainst changedThen changed
        vecBodyElse
            <-  Vec.Bind vecStatementElse
            <$> vectorizeAgainst changedElse changed
        let vecIf = Vec.AppOp3 Vec.OpIf vecBodyElse vecBodyThen vecCond
        return (changed, vecIf)

vectorizeAtom :: Atom ~> Vec.Expression
vectorizeAtom = \case
    Primary a -> return (Vec.Primary (vectorizeLiteral a))
    Access name -> mkAccess name

vectorizePattern :: Pattern ~> Vec.Pattern
vectorizePattern = \case
    PUnit -> return Vec.PUnit
    PWildCard -> return Vec.PWildCard
    PAccess name -> mkPAccess name
    PTuple p1 p2 -> Vec.PTuple <$> vectorizePattern p1 <*> vectorizePattern p2

lookupIndex :: Name ~> Index
lookupIndex = lookupX (view vsIndices)

lookupType :: Name ~> Type
lookupType = lookupX (view vsTypes)

lookupName :: Name -> Index ~> Maybe Vec.Name
lookupName name index
      = views vsNames (M.lookup (name, index))
    >>= (throwMaybe.SomeException) (ErrorNoAccess name [])

lookupX :: (VectorizeScope -> Map Name x) -> Name ~> x
lookupX f name = do
    xs <- asks f
    M.lookup name xs & (throwMaybe.SomeException) (ErrorNoAccess name (M.keys xs))

initIndices :: E m => Index -> Map Name Type -> m VectorizeScope
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
