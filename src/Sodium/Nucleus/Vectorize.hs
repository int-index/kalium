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
        vecFuncLambda = Vec.lambda (map mkParam params) vecBody
    return $ Vec.Func vecFuncSig vecFuncLambda

mkPatTuple [  ] = Vec.PUnit
mkPatTuple pats = foldr1 Vec.PTuple pats

mkExpTuple [  ] = Vec.Atom $ Vec.Primary (Lit STypeUnit ())
mkExpTuple exps = foldr1 (\a b -> Vec.Atom (Vec.OpAccess OpPair) `Vec.App` a `Vec.App` b) (map Vec.Atom exps)

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

mkAccess :: V e t m => Name -> t m Vec.Atom
mkAccess name = smartAccess name <$> lookupIndex name

expTuple :: V e t m => [Name] -> t m Vec.Expression
expTuple names = mkExpTuple <$> mapM mkAccess names

patTuple :: V e t m => [Name] -> t m Vec.Pattern
patTuple names = mkPatTuple <$> mapM mkPAccess names

vectorizeBody :: (V e t m, Scoping v) => Scope v Body Pattern Atom -> t m ([Name], Vec.Expression)
vectorizeBody scope = do
    let vars = scope ^. scopeVars
    let Body statement result = scope ^. scopeElem
    (changed, vecBodyGen) <- vectorizeScope (Scope vars statement)
    vecBody <- lift $ vecBodyGen [result]
    return (changed, vecBody)

updateLocalize names action = do
    updated <- forM names $ \name -> do
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
                  vecResult <- Vec.taint . mkExpTuple <$> mapM vectorizeAtom results
                  return $ Vec.follow pat vecStatement vecResult
            let changedNonlocal = filter (`M.notMember` vars) changed
            return (changedNonlocal, vecBodyGen)

vectorizeStatement :: V e t m => Statement Pattern Atom -> t m ([Name], Vec.Expression)
vectorizeStatement = \case
    Pass -> return ([], Vec.taint $ Vec.Atom $ Vec.Primary (Lit STypeUnit ()))
    Follow st1 st2 -> do
        (changed1, vecStatement1) <- vectorizeStatement st1
        updateLocalize changed1 $ do
            vecPat1 <- patTuple changed1
            (changed2, vecStatement2) <- vectorizeStatement st2
            updateLocalize changed2 $ do
                vecPat2 <- patTuple changed2
                let changed = nub $ changed1 ++ changed2
                results <- Vec.taint . mkExpTuple <$> mapM vectorizeAtom (map Access changed)
                let vecBody = Vec.follow vecPat1 vecStatement1
                            $ Vec.follow vecPat2 vecStatement2
                            $ results
                return (changed, vecBody)
    ScopeStatement scope -> do
        (changed, vecBodyGen) <- vectorizeScope scope
        vecBody <- lift $ vecBodyGen (map Access changed)
        return (changed, vecBody)
    Execute (Exec pat name args) -> do
        -- TODO: purity flag in function signature
        let impure = case name of
              NameOp OpReadLn  -> True
              NameOp OpGetLn      -> True
              NameOp OpPrintLn    -> True
              NameOp OpPutLn -> True
              Name1 _ _ -> True
              _ -> False
        vecArgs <- mapM vectorizeAtom args
        let patFlatten = \case
              PWildCard -> []
              PUnit -> []
              PAccess name -> [name]
              PTuple p1 p2 -> patFlatten p1 ++ patFlatten p2
        let changed = patFlatten pat
        updateLocalize changed $ do
            vecPattern <- vectorizePattern pat
            results <- Vec.taint . mkExpTuple <$> mapM vectorizeAtom (map Access changed)
            let vecTaint = if impure then id else Vec.taint
                vecCall = foldl1 Vec.App $ map Vec.Atom$ Vec.Access (retag name):vecArgs
                vecExecute = vecTaint vecCall
                vecBody = Vec.follow vecPattern vecExecute results
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
            let vecFor = Vec.Atom (Vec.OpAccess Vec.OpFoldTainted)
                       `Vec.App` vecLambda `Vec.App` argExp
                       `Vec.App` Vec.Atom vecRange
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
        let vecIf = Vec.Atom (Vec.OpAccess Vec.OpIf) `Vec.App` vecBodyElse
                  `Vec.App` vecBodyThen `Vec.App` Vec.Atom vecCond
        return (changed, vecIf)

vectorizeAtom :: V e t m => Atom -> t m Vec.Atom
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
