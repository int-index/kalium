{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
 
module Sodium.Pascal.Convert (convert, Error(..)) where

import qualified Data.Map as M
import Data.Monoid
import Data.Traversable

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Lens
-- S for Src, D for Dest
import qualified Sodium.Pascal.Program as S
import qualified Sodium.Nucleus.Scalar.Program as D
import qualified Sodium.Nucleus.Scalar.Build   as D
import Sodium.Util

declareLenses [d|

    data TypeScope = TypeScope
        { tsFunctions :: M.Map S.Name S.FuncSig
        , tsVariables :: M.Map S.Name S.Type
        } deriving (Eq)

    data ConvScope = ConvScope
        { csTypes :: TypeScope
        , csNames :: M.Map (Bool, S.Name) D.Name
        } deriving (Eq)

                |]

instance Monoid TypeScope where
    mempty = TypeScope mempty mempty
    ts1 `mappend` ts2 = TypeScope
        (ts1 ^. tsFunctions <> ts2 ^. tsFunctions)
        (ts1 ^. tsVariables <> ts2 ^. tsVariables)

instance Monoid ConvScope where
    mempty = ConvScope mempty mempty
    cs1 `mappend` cs2 = ConvScope
        (cs1 ^. csTypes <> cs2 ^. csTypes)
        (cs1 ^. csNames <> cs2 ^. csNames)

class Error e where
    errorTypecheck  :: e
    errorNoAccess   :: e
    errorNoFunction :: e

type Erroneous e m = (MonadError e m, Error e)

convert :: (Applicative m, MonadSupply Integer m, Erroneous e m)
        => S.Program -> m (D.Program D.ByType D.Pattern D.Expression)
convert program = runReaderT (conv program) mempty

--nameV name = return $ D.NameGen 42 ("v_" ++ name)
nameV name = lookupName (False, name)
nameF name = lookupName (True, name)

lookupName :: (Applicative m, MonadReader ConvScope m, Erroneous e m)
           => (Bool, S.Name) -> m D.Name
lookupName k = do
    mname <- views csNames (M.lookup k)
    maybe (throwError errorNoAccess) return mname

class Conv s d | s -> d where
    conv :: (Applicative m, MonadSupply Integer m, Erroneous e m)
         => s -> ReaderT ConvScope m d

instance Conv S.Program (D.Program D.ByType D.Pattern D.Expression) where
    conv (S.Program funcs vars body) = do
        (M.unions -> funcNames) <- for funcs $ \(S.Func name _ _ _) -> do
            funcName <- D.NameGen <$> supply
            return $ M.singleton (True, name) funcName
        local ( (csTypes . tsFunctions %~ M.union funcSigs)
              . (csNames %~ M.union funcNames)
              ) $ do
            clMain <- do
                clBody <- convScope vars
                    $ D.Body <$> conv body <*> pure (D.expression ())
                let noparams = D.Scope ([] :: Pairs D.Name D.ByType)
                return $ D.Func D.TypeUnit (noparams clBody)
            clFuncs <- traverse conv funcs
            return $ D.Program (M.fromList $ (D.NameSpecial D.OpMain, clMain):clFuncs)
        where funcSigs = M.unions (map funcSigOf funcs)
              funcSigOf (S.Func name funcSig _ _) = M.singleton name funcSig

convScope vardecls inner = do
    (M.unions -> varNames, scopeVars)
        <- unzip <$> traverse convVardecl (M.toList vardecls)
    scopeElem <- local ( (csTypes . tsVariables %~ M.union vardecls)
                       . (csNames %~ M.union varNames)
                       ) inner
    return $ D.Scope (D.scoping scopeVars) scopeElem
       where convVardecl (name, pasType) = do
                varName <- D.NameGen <$> supply
                ty <- conv pasType
                return (M.singleton (False, name) varName, (varName, ty))

convScope' paramdecls inner = do
    (M.unions -> paramNames, scopeVars)
        <- unzip <$> traverse convParamdecl paramdecls
    scopeElem <- local ( (csTypes . tsVariables %~ M.union vardecls)
                       . (csNames %~ M.union paramNames)
                       ) inner
    return $ D.Scope scopeVars scopeElem
       where paramDeclToTup (S.ParamDecl name (_, ty)) = (name, ty)
             vardecls = M.fromList $ map paramDeclToTup paramdecls
             convParamdecl (S.ParamDecl name (r, pasType)) = do
                paramName <- D.NameGen <$> supply
                r' <- conv r
                ty <- conv pasType
                return (M.singleton (False, name) paramName, (paramName, (r', ty)))

instance Conv S.Body (D.Statement D.Pattern D.Expression) where
    conv statements = D.group <$> traverse conv statements

instance Conv S.Func (D.Name, D.Func D.ByType D.Pattern D.Expression) where
    conv (S.Func name (S.FuncSig params pasType) vars body) = case pasType of
        Nothing -> do
            clScope <- convScope' params
                     $ convScope  vars
                     $ D.Body <$> conv body <*> pure (D.expression ())
            fname <- nameF name
            return $ (fname, D.Func D.TypeUnit clScope)
        Just ty -> do
            let retVars = M.singleton name ty
            clScope <- convScope' params
                     $ convScope (M.union vars retVars)
                     $ D.Body <$> conv body <*> (D.Atom . D.Access <$> nameV name)
            retType <- conv ty
            fname <- nameF name
            return (fname, D.Func retType clScope)
    
instance Conv S.By D.By where
    conv S.ByValue     = pure D.ByValue
    conv S.ByReference = pure D.ByReference

instance Conv S.Type D.Type where
    conv = \case
        S.TypeInteger -> return D.TypeInteger
        S.TypeReal    -> return D.TypeDouble
        S.TypeBoolean -> return D.TypeBoolean
        S.TypeChar    -> return D.TypeChar
        S.TypeString  -> return (D.TypeList D.TypeChar)
        S.TypeArray t -> D.TypeList <$> conv t
        S.TypeCustom _  -> error "Custom types are not implemented"

binary op a b = D.Call op [] [a,b]

convReadLn [e@(S.Access name')] = do
    name <- nameV name'
    typecheck e >>= \case
        S.TypeString -> return $ D.Exec (D.PAccess name) (D.NameSpecial D.OpGetLn) [] []
        ty' -> do
            ty <- conv ty'
            return $ D.Exec (D.PAccess name) (D.NameSpecial D.OpReadLn) [ty] []
convReadLn _ = error "IOMagic supports only single-value read operations"

convWriteLn exprs = do
    let convArg expr = do
          ty <- typecheck expr
          let wrap = case ty of
                S.TypeString -> id
                S.TypeChar -> \e -> D.Call (D.NameSpecial D.OpSingleton) [] [e]
                _ -> \e -> D.Call (D.NameSpecial D.OpShow) [] [e]
          wrap <$> conv expr
    arg <- traverse convArg exprs <&> \case
        [] -> D.expression ""
        args -> foldl1 (binary (D.NameSpecial D.OpConcat)) args
    return $ D.Exec D.PUnit (D.NameSpecial D.OpPutLn) [] [arg]

typeOfLiteral :: S.Literal -> S.Type
typeOfLiteral = \case
    S.LitBool _ -> S.TypeBoolean
    S.LitInt  _ -> S.TypeInteger
    S.LitReal _ -> S.TypeReal
    S.LitChar _ -> S.TypeChar
    S.LitStr  _ -> S.TypeString

typeOfAccess :: (Applicative m, MonadReader ConvScope m, Erroneous e m)
             => S.Name -> m S.Type
typeOfAccess name = do
    mtype <- views (csTypes.tsVariables) (M.lookup name)
    maybe (throwError errorNoAccess) return mtype

typecasts :: (Applicative m, MonadReader ConvScope m, Erroneous e m)
          => S.Expression -> m [S.Expression]
typecasts expr@(S.Primary lit) = return $ typecasting (typeOfLiteral lit) expr
typecasts expr@(S.Access name) = do
    ty <- typeOfAccess name
    return $ typecasting ty expr
typecasts (S.Call nameOp args) = do
    possibleArgs <- traverse typecasts args
    let calls = S.Call nameOp <$> sequenceA possibleArgs
    niceCalls <- traverse typechecking calls
    return (concat niceCalls)
        where typechecking expr =  maybe empty (\_ -> pure expr)
                               <$> typecheck' expr

typecasting :: S.Type -> S.Expression -> [S.Expression]
typecasting ty expr = expr : [op1App tc expr | tc <- tcs]
    where tcs = case ty of
            S.TypeChar    -> [S.OpCharToString]
            S.TypeInteger -> [S.OpIntToReal]
            _ -> []

typecheck expr = do
    mty <- typecheck' expr
    maybe (throwError errorTypecheck) return mty

typecheck' :: (Applicative m, MonadReader ConvScope m, Erroneous e m)
          => S.Expression -> m (Maybe S.Type)
typecheck' = runMaybeT . \case
    S.Primary lit -> return (typeOfLiteral lit)
    S.Access name -> typeOfAccess name
    S.Call (Right name) _ -> do -- TODO: check argument types
        mfuncsig <- views (csTypes.tsFunctions) (M.lookup name)
        case mfuncsig of
            Nothing -> throwError errorNoFunction
            Just (S.FuncSig _ mtype) -> case mtype of
                Nothing -> badType
                Just t -> return t
    S.Call (Left op) args -> do
        tys <- traverse typecheck args
        let isNumeric = liftA2 (||) (== S.TypeInteger) (== S.TypeReal)
        case (op, tys) of
            (S.OpAdd     , [t1, t2]) | t1 == t2, isNumeric t1 || t1 == S.TypeString -> return t1
            (S.OpSubtract, [t1, t2]) | t1 == t2, isNumeric t1 -> return t1
            (S.OpMultiply, [t1, t2]) | t1 == t2, isNumeric t1 -> return t1
            (S.OpDivide  , [S.TypeReal   , S.TypeReal   ]) -> return S.TypeReal
            (S.OpDiv     , [S.TypeInteger, S.TypeInteger]) -> return S.TypeInteger
            (S.OpMod     , [S.TypeInteger, S.TypeInteger]) -> return S.TypeInteger
            (S.OpLess    , [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpMore    , [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpEquals  , [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpAnd     , [S.TypeBoolean, S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpOr      , [S.TypeBoolean, S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpXor     , [S.TypeBoolean, S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpNegate  , [t1]) | isNumeric t1 -> return t1
            (S.OpPlus    , [t1]) | isNumeric t1 -> return t1
            (S.OpNot     , [S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpCharToString, [S.TypeChar   ]) -> return S.TypeString
            (S.OpIntToReal   , [S.TypeInteger]) -> return S.TypeReal
            _ -> badType
    where badType = MaybeT (return Nothing)

op1App :: S.Operator -> S.Expression -> S.Expression
op1App op e = S.Call (Left op) [e]

typecastConv ty expr = do
    tcs <- typecasts expr
    tc <- filterM (\tc -> (==) ty <$> typecheck tc) tcs >>= \case
        [] -> throwError errorTypecheck
        tc:_ -> return tc
    conv tc

instance Conv S.Statement (D.Statement D.Pattern D.Expression) where
    conv = \case
        S.BodyStatement body -> D.statement <$> conv body
        S.Assign name' expr' -> do
            name <- nameV name'
            tyW <- typecheck (S.Access name')
            expr <- typecastConv tyW expr'
            return $ D.assign name expr
        S.Execute "readln"  exprs -> D.statement <$> convReadLn  exprs
        S.Execute "writeln" exprs -> D.statement <$> convWriteLn exprs
        S.Execute name' exprs' -> do
            name <- nameF name'
            exprs <- traverse conv exprs'
            return $ D.statement $ D.Exec D.PWildCard name [] exprs
        S.ForCycle name fromExpr toExpr statement -> do
            clName <- nameV name
            clFromExpr <- conv fromExpr
            clToExpr   <- conv toExpr
            let clRange = binary (D.NameSpecial D.OpRange) clFromExpr clToExpr
            clAction <- conv statement
            let clForCycle = D.statement (D.ForCycle clName clRange clAction)
            return $ D.group [clForCycle, D.assign clName clToExpr]
        S.IfBranch expr bodyThen mBodyElse
             -> fmap D.statement
             $  D.If
            <$> typecastConv S.TypeBoolean expr
            <*> conv bodyThen
            <*> (D.statements <$> traverse conv mBodyElse)
        S.CaseBranch expr leafs mBodyElse -> do
            clType <- typecheck expr >>= conv
            clExpr <- conv expr
            clName <- D.NameGen <$> supply
            let clCaseExpr = D.expression clName
            let instRange = \case
                    Right (exprFrom, exprTo)
                         ->  binary (D.NameSpecial D.OpElem) clCaseExpr
                        <$> (binary (D.NameSpecial D.OpRange) <$> conv exprFrom <*> conv exprTo)
                    Left expr -> binary (D.NameSpecial D.OpEquals) clCaseExpr <$> conv expr
            let instLeaf (exprs, body)
                     =  (,)
                    <$> (foldl1 (binary (D.NameSpecial D.OpOr)) <$> traverse instRange exprs)
                    <*> conv body
            leafs <- traverse instLeaf leafs
            leafElse <- D.statements <$> traverse conv mBodyElse
            let statement = foldr
                    (\(cond, ifThen) ifElse ->
                        D.statement $ D.If cond ifThen ifElse)
                     leafElse leafs
            return $ D.statement $ D.Scope
                        (M.singleton clName clType)
                        (D.group [D.assign clName clExpr, statement])

instance Conv S.Expression D.Expression where
    -- TODO: use typecastConv everywhere
    conv = \case
        S.Access name -> D.expression <$> nameV name
        S.Call name' exprs -> do
            name <- case name' of
                Left op -> conv op
                Right name -> nameF name
            D.Call name [] <$> traverse conv exprs
        S.Primary lit -> conv lit

instance Conv S.Literal D.Expression where
    conv = \case
        S.LitInt  x -> return (D.expression x)
        S.LitReal x -> return (D.expression x)
        S.LitStr  x -> return (D.expression x)
        S.LitChar x -> return (D.expression x)
        S.LitBool x -> return (D.expression x)

instance Conv S.Operator D.Name where
    conv = return . D.NameSpecial . \case
        S.OpAdd -> D.OpAdd
        S.OpSubtract -> D.OpSubtract
        S.OpMultiply -> D.OpMultiply
        S.OpDivide -> D.OpDivide
        S.OpDiv  -> D.OpDiv
        S.OpMod  -> D.OpMod
        S.OpLess -> D.OpLess
        S.OpMore -> D.OpMore
        S.OpEquals -> D.OpEquals
        S.OpAnd -> D.OpAnd
        S.OpOr  -> D.OpOr
        S.OpXor -> D.OpXor
        S.OpPlus   -> D.OpId
        S.OpNegate -> D.OpNegate
        S.OpNot    -> D.OpNot
        S.OpCharToString -> D.OpSingleton
        S.OpIntToReal    -> D.OpIntToDouble
