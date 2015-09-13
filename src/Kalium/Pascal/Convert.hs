{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Kalium.Pascal.Convert (convert) where

import Kalium.Prelude
import Kalium.Util

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Rename

import Control.Exception
import Control.Lens (ix)
import qualified Data.Map as M

import Control.Monad.Trans.Maybe
-- S for Src, D for Dest
import qualified Kalium.Pascal.Program as S
import qualified Kalium.Nucleus.Scalar.Program as D
import qualified Kalium.Nucleus.Scalar.Build   as D

declareLenses [d|

    data TypeScope = TypeScope
        { tsFunctions :: Map S.Name S.FuncSig
        , tsVariables :: Map S.Name S.Type
        } deriving (Eq)

    data ConvScope = ConvScope
        { csTypes :: TypeScope
        , csNames :: Map Bool (Map S.Name D.Name)
        } deriving (Eq)

                |]

instance Exception ErrorTypecheck
data ErrorTypecheck = ErrorTypecheck
    deriving (Show)

instance Exception ErrorNoAccess
data ErrorNoAccess = ErrorNoAccess String [String]
    deriving (Show)

instance Exception ErrorNoFunction
data ErrorNoFunction = ErrorNoFunction String
    deriving (Show)

instance Exception ErrorNotImplemented
data ErrorNotImplemented = ErrorNotImplemented String
    deriving (Show)

instance Exception ErrorArgumentMismatch
data ErrorArgumentMismatch = ErrorArgumentMismatch String
    deriving (Show)

type E m = MonadError SomeException m
type (~>)  a b = forall m . (MonadReader ConvScope m, E m) => Kleisli' m a b
type (~>.) a b = forall m . (MonadReader ConvScope m, E m, MonadNameGen m) => Kleisli' m a b

convert :: (E m, MonadNameGen m) => S.Program -> m (D.Complex D.Program)
convert program = do
    let initScope = ConvScope (TypeScope mempty mempty)
                              (M.fromList (liftA2(,)[True,False][mempty]))
    runReaderT (conv program) initScope

nameV, nameF :: S.Name ~> D.Name
nameV = lookupName False
nameF = lookupName True

lookupName :: Bool -> S.Name ~> D.Name
lookupName ct name = do
    names <- views csNames (M.! ct)
    let mname = M.lookup name names
    (throwMaybe.SomeException) (ErrorNoAccess name (M.keys names)) mname

alias :: MonadNameGen m => Kleisli' m S.Name D.Name
alias name = D.NameGen <$> mkname (Just name)

opUnit  = D.Call (D.NameSpecial D.OpUnit)  [] []
opTrue  = D.Call (D.NameSpecial D.OpTrue)  [] []
opFalse = D.Call (D.NameSpecial D.OpFalse) [] []
opNil t = D.Call (D.NameSpecial D.OpNil)  [t] []
opCons x xs = D.Call (D.NameSpecial D.OpCons) [] [x, xs]

opAssign :: D.Name -> a -> D.Statement (D.Configuration param D.Pattern a)
opAssign name a = D.statement $ D.Exec (D.PAccess name) (D.NameSpecial D.OpId) [] [a]

class Conv s where
    type Scalar s :: *
    conv :: s ~>. Scalar s

instance Conv S.Program where
    type Scalar S.Program = D.Complex D.Program
    conv (S.Program funcs vars body) = do
        (mconcat -> funcNames) <- for funcs $ \(S.Func name _ _ _) -> do
            funcName <- alias name
            return $ M.singleton name funcName
        local ( (csTypes . tsFunctions %~ mappend funcSigs)
              . (csNames %~ M.adjust (mappend funcNames) True)
              ) $ do
            clMain <- do
                clBody <- convScope vars
                    $ D.Body <$> conv body <*> pure opUnit
                let noparams = D.Scope ([] :: Pairs D.Name D.ByType)
                return $ D.Func D.TypeUnit (noparams clBody)
            clFuncs <- traverse conv funcs
            let programFuncs = (D.NameSpecial D.OpMain, clMain):clFuncs
            return $ D.Program (M.fromList programFuncs)
        where funcSigs = mconcat (map funcSigOf funcs)
              funcSigOf (S.Func name funcSig _ _) = M.singleton name funcSig

convScope vardecls inner = do
    (mconcat -> varNames, scopeVars)
        <- unzip <$> traverse convVardecl (M.toList vardecls)
    scopeElem <- local ( (csTypes . tsVariables %~ mappend vardecls)
                       . (csNames %~ M.adjust (mappend varNames) False)
                       ) inner
    return $ D.Scope (D.scoping scopeVars) scopeElem
       where convVardecl (name, pasType) = do
                varName <- alias name
                ty <- conv pasType
                return (M.singleton name varName, (varName, ty))

convScope' paramdecls inner = do
    (mconcat -> paramNames, scopeVars)
        <- unzip <$> traverse convParamdecl paramdecls
    scopeElem <- local ( (csTypes . tsVariables %~ mappend vardecls)
                       . (csNames %~ M.adjust (mappend paramNames) False)
                       ) inner
    return $ D.Scope scopeVars scopeElem
       where paramDeclToTup (S.ParamDecl name (_, ty)) = (name, ty)
             vardecls = M.fromList $ map paramDeclToTup paramdecls
             convParamdecl (S.ParamDecl name (r, pasType)) = do
                paramName <- alias name
                r' <- conv r
                ty <- conv pasType
                return (M.singleton name paramName, (paramName, (r', ty)))

instance Conv S.Body where
    type Scalar S.Body = D.Complex D.Statement
    conv statements = D.follow <$> traverse conv statements

instance Conv S.Func where
    type Scalar S.Func = (D.Name, D.Complex D.Func)
    conv (S.Func name (S.FuncSig params pasType) vars body) = case pasType of
        Nothing -> do
            clScope <- convScope' params
                     $ convScope  vars
                     $ D.Body <$> conv body <*> pure opUnit
            fname <- nameF name
            return $ (fname, D.Func D.TypeUnit clScope)
        Just ty -> do
            let retVars = M.singleton name ty
            clScope <- convScope' params
                     $ convScope (vars <> retVars)
                     $ D.Body <$> conv body <*> (D.Atom . D.Access <$> nameV name)
            retType <- conv ty
            fname <- nameF name
            return (fname, D.Func retType clScope)

instance Conv S.By where
    type Scalar S.By = D.By
    conv S.ByValue     = pure D.ByValue
    conv S.ByReference = pure D.ByReference

instance Conv S.Type where
    type Scalar S.Type = D.Type
    conv = \case
        S.TypeInteger -> return D.TypeInteger
        S.TypeReal    -> return D.TypeDouble
        S.TypeBoolean -> return D.TypeBoolean
        S.TypeChar    -> return D.TypeChar
        S.TypeString  -> return D.TypeString
        S.TypeArray t -> D.TypeApp1 D.TypeList <$> conv t
        S.TypeCustom _  -> (throwError.SomeException) (ErrorNotImplemented "Custom types")

binary op a b = D.Call op [] [a,b]

convSetLength [S.Access name', lenExpr'] = do
    name <- nameV name'
    lenExpr <- typecastConv (==S.TypeInteger) lenExpr'
    return $ D.Exec (D.PAccess name) (D.NameSpecial D.OpSetLength) []
        [D.expression name, lenExpr]
convSetLength _ = (throwError.SomeException) (ErrorArgumentMismatch "SetLength")

convReadLn [e@(S.Access name')] = do
    name <- nameV name'
    typecheck e >>= \case
        S.TypeString -> return $ D.Exec (D.PAccess name) (D.NameSpecial D.OpGetLn) [] []
        ty' -> do
            ty <- conv ty'
            return $ D.Exec (D.PAccess name) (D.NameSpecial D.OpReadLn) [ty] []
convReadLn _ = (throwError.SomeException) (ErrorArgumentMismatch "ReadLn")

convRead [e@(S.Access name')] = do
    name <- nameV name'
    typecheck e >>= \case
        S.TypeChar -> return $ D.Exec (D.PAccess name) (D.NameSpecial D.OpGetChar) [] []
        _ -> (throwError.SomeException) ErrorTypecheck
convRead _ = (throwError.SomeException) (ErrorArgumentMismatch "Read")

convWriteLn ln exprs = do
    arg <- traverse convArg exprs <&> \case
        [] -> opNil D.TypeChar
        args -> foldr1 (binary (D.NameSpecial D.OpConcat)) args
    let op | ln = D.NameSpecial D.OpPutLn
           | otherwise = D.NameSpecial D.OpPut
    return $ D.Exec D.PUnit op [] [arg]
  where
    convArg expr = do
        tcs <- typecasts expr
        case keepByFst (==S.TypeString) tcs of
            tc:_ -> convExpr tc
            [] -> case keepByFst (==S.TypeChar) tcs of
                tc:_ -> do
                    e <- convExpr tc
                    return $ D.Call (D.NameSpecial D.OpSingleton) [] [e]
                [] -> case listToMaybe (map snd tcs) of
                    Nothing -> (throwError.SomeException) ErrorTypecheck
                    Just expr' -> do
                        e <- convExpr expr'
                        return $ D.Call (D.NameSpecial D.OpShow) [] [e]

typeOfLiteral :: S.Literal -> S.Type
typeOfLiteral = \case
    S.LitBool _ -> S.TypeBoolean
    S.LitInt  _ -> S.TypeInteger
    S.LitReal _ -> S.TypeReal
    S.LitChar _ -> S.TypeChar
    S.LitStr  _ -> S.TypeString

typeOfAccess :: S.Name ~> S.Type
typeOfAccess name = do
    types <- view (csTypes.tsVariables)
    let mtype = M.lookup name types
    (throwMaybe.SomeException) (ErrorNoAccess name (M.keys types)) mtype

typecasts :: S.Expression ~> Pairs S.Type S.Expression
typecasts expr = case expr of
    S.Primary lit -> nice $ typecasting (typeOfLiteral lit) expr
    S.Access name -> do
        ty <- typeOfAccess name
        nice $ typecasting ty expr
    S.Call nameOp args -> do
        possibleArgs <- traverse typecasts' args
        let calls = S.Call nameOp <$> sequenceA possibleArgs
        nice calls
  where
    nice :: [S.Expression] ~> Pairs S.Type S.Expression
    nice exprs = join <$> traverse typechecking exprs
    typecasts' expr = map snd <$> typecasts expr

typechecking :: S.Expression ~> Pairs S.Type S.Expression
typechecking expr = maybe empty (\ty -> pure (ty, expr)) <$> typecheck' expr

typecasting :: S.Type -> S.Expression -> [S.Expression]
typecasting ty expr = expr : [op1App tc expr | tc <- tcs]
    where tcs = case ty of
            S.TypeChar    -> [S.OpCharToString]
            S.TypeInteger -> [S.OpIntToReal]
            _ -> []

typecheck :: S.Expression ~> S.Type
typecheck expr = typecheck' expr >>= (throwMaybe.SomeException) ErrorTypecheck

typecheck' :: S.Expression ~> Maybe S.Type
typecheck' = runMaybeT . \case
    S.Primary lit -> return (typeOfLiteral lit)
    S.Access name -> typeOfAccess name
    S.Call (Right "chr") args -> do
        traverse typecheck args >>= \case
            [S.TypeInteger] -> return S.TypeChar
            _ -> badType
    S.Call (Right "ord") args -> do
        traverse typecheck args >>= \case
            [S.TypeChar] -> return S.TypeInteger
            _ -> badType
    S.Call (Right "length") args -> do
        traverse typecheck args >>= \case
            [S.TypeString ] -> return S.TypeInteger
            [S.TypeArray _] -> return S.TypeInteger
            _ -> badType
    S.Call (Right name) args -> do
        mfuncsig <- views (csTypes.tsFunctions) (M.lookup name)
        case mfuncsig of
            Nothing -> (throwError.SomeException) (ErrorNoFunction name)
            Just (S.FuncSig params mtype) -> case mtype of
                Nothing -> badType
                Just t -> do
                    let tys = params & map (\(S.ParamDecl _ (_, ty)) -> ty)
                    (sequenceA -> mtyArgs) <- traverse typecheck' args
                    case mtyArgs of
                        Just tyArgs | tyArgs == tys -> return t
                        _ -> badType
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
            (S.OpLessEquals, [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpMoreEquals, [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpEquals  , [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpNotEquals, [t1, t2]) | t1 == t2 -> return S.TypeBoolean
            (S.OpAnd     , [S.TypeBoolean, S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpOr      , [S.TypeBoolean, S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpXor     , [S.TypeBoolean, S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpNegate  , [t1]) | isNumeric t1 -> return t1
            (S.OpPlus    , [t1]) | isNumeric t1 -> return t1
            (S.OpNot     , [S.TypeBoolean]) -> return S.TypeBoolean
            (S.OpIx      , [S.TypeString  , S.TypeInteger]) -> return S.TypeChar
            (S.OpIx      , [S.TypeArray t1, S.TypeInteger]) -> return t1
            (S.OpCharToString, [S.TypeChar   ]) -> return S.TypeString
            (S.OpIntToReal   , [S.TypeInteger]) -> return S.TypeReal
            _ -> badType
    where badType = MaybeT (return Nothing)

op1App :: S.Operator -> S.Expression -> S.Expression
op1App op e = S.Call (Left op) [e]

typecastConv :: (S.Type -> Bool) -> S.Expression ~>. D.Expression
typecastConv p expr = do
    tcs <- typecasts expr
    tc <- case keepByFst p tcs of
        [] -> (throwError.SomeException) ErrorTypecheck
        tc:_ -> return tc
    convExpr tc

typecastConv' :: S.Expression ~>. D.Expression
typecastConv' = typecastConv (const True)

instance Conv S.Statement where
    type Scalar S.Statement = D.Complex D.Statement
    conv = \case

        S.BodyStatement body -> D.statement <$> conv body

        S.Assign name' mIxExpr' expr' ->
            case mIxExpr' of
                Nothing -> do
                    name <- nameV name'
                    tyW <- typecheck (S.Access name')
                    expr <- typecastConv (==tyW) expr'
                    return $ opAssign name expr
                Just ixExpr' -> do
                    ixExpr <- typecastConv (==S.TypeInteger) ixExpr'
                    name <- nameV name'
                    tyW <- typecheck (S.Access name') >>= \case
                        S.TypeString ->
                            (throwError.SomeException)
                            (ErrorNotImplemented "String indexing")
                        S.TypeArray ty -> return ty
                        _ -> (throwError.SomeException) ErrorTypecheck
                    elemExpr <- typecastConv (==tyW) expr'
                    let expr = D.Call (D.NameSpecial D.OpIxSet) []
                                [ ixExpr, elemExpr
                                , D.expression (D.Access name) ]
                    return $ opAssign name expr

        S.Execute "read"    exprs -> D.statement <$> convRead exprs
        S.Execute "readln"  exprs -> D.statement <$> convReadLn  exprs
        S.Execute "write"   exprs -> D.statement <$> convWriteLn False exprs
        S.Execute "writeln" exprs -> D.statement <$> convWriteLn True  exprs
        S.Execute "setlength" exprs -> D.statement <$> convSetLength exprs
        S.Execute name' exprs' -> do
            name <- nameF name'
            exprs <- traverse typecastConv' exprs'
            return $ D.statement $ D.Exec D.PWildCard name [] exprs

        S.ForCycle name fromExpr toExpr statement -> do
            clName <- nameV name
            clFromExpr <- typecastConv' fromExpr
            clToExpr   <- typecastConv' toExpr
            let clRange = binary (D.NameSpecial D.OpRange) clFromExpr clToExpr
            clAction <- conv statement
            let clForCycle = D.statement (D.ForCycle clName clRange clAction)
            return $ D.follow [clForCycle, opAssign clName clToExpr]

        S.IfBranch expr bodyThen mBodyElse
             -> fmap D.statement
             $  D.If
            <$> typecastConv (==S.TypeBoolean) expr
            <*> conv bodyThen
            <*> (D.statements <$> traverse conv mBodyElse)

        S.CaseBranch expr leafs mBodyElse -> do
            clType <- typecheck expr >>= conv
            clExpr <- typecastConv' expr
            clName <- alias "case"
            let clCaseExpr = D.expression clName
            let instRange = \case
                    Right (exprFrom, exprTo)
                         ->  binary (D.NameSpecial D.OpElem) clCaseExpr
                        <$> (binary (D.NameSpecial D.OpRange)
                            <$> typecastConv' exprFrom <*> typecastConv' exprTo)
                    Left expr
                         -> binary (D.NameSpecial D.OpEquals) clCaseExpr
                        <$> typecastConv' expr
            let instLeaf (exprs, body)
                     =  (,)
                    <$> (foldr1 (binary (D.NameSpecial D.OpOr)) <$> traverse instRange exprs)
                    <*> conv body
            leafs <- traverse instLeaf leafs
            leafElse <- D.statements <$> traverse conv mBodyElse
            let statement = foldr
                    (\(cond, ifThen) ifElse ->
                        D.statement $ D.If cond ifThen ifElse)
                     leafElse leafs
            return $ D.statement $ D.Scope
                        (M.singleton clName clType)
                        (D.follow [opAssign clName clExpr, statement])

-- use typecastConv to get typecasting
convExpr :: S.Expression ~>. D.Expression
convExpr = \case
    S.Access name -> D.expression <$> nameV name
    S.Call name' exprs -> do
        let direct' op = D.Call op [] <$> traverse convExpr exprs
        let direct = direct' . D.NameSpecial
        case name' of
            Left S.OpAdd -> do
                traverse typecheck' exprs >>= \case
                    Just S.TypeString : _ -> direct D.OpConcat
                    _ -> direct D.OpAdd
            Left S.OpSubtract -> direct D.OpSubtract
            Left S.OpMultiply -> direct D.OpMultiply
            Left S.OpDivide -> direct D.OpDivide
            Left S.OpDiv  -> direct D.OpDiv
            Left S.OpMod  -> direct D.OpMod
            Left S.OpLess -> direct D.OpLess
            Left S.OpMore -> direct D.OpMore
            Left S.OpLessEquals -> direct D.OpLessEquals
            Left S.OpMoreEquals -> direct D.OpMoreEquals
            Left S.OpNotEquals  -> direct D.OpNotEquals
            Left S.OpEquals -> direct D.OpEquals
            Left S.OpAnd -> direct D.OpAnd
            Left S.OpOr  -> direct D.OpOr
            Left S.OpXor -> direct D.OpXor
            Left S.OpPlus   -> direct D.OpId
            Left S.OpNegate -> direct D.OpNegate
            Left S.OpNot    -> direct D.OpNot
            Left S.OpIx  -> do
                let sub e a = D.Call (D.NameSpecial D.OpSubtract) [] [a, e]
                    one = D.expression (D.LitInteger 1)
                traverse typecheck' exprs >>= \case
                    Just S.TypeString : _ -> D.Call (D.NameSpecial D.OpIx) []
                       <$> (traverse convExpr exprs <&> ix 1 %~ sub one)
                    _  -> direct D.OpIx
            Left S.OpCharToString -> direct D.OpSingleton
            Left S.OpIntToReal    -> direct D.OpIntToDouble
            Right "length" -> direct D.OpLength
            Right "chr" -> direct D.OpChr
            Right "ord" -> direct D.OpChrOrd
            Right name  -> nameF name >>= direct'
    S.Primary lit -> conv lit

convLiteral = \case
    S.LitInt  x -> D.expression (D.LitInteger x)
    S.LitReal x -> D.expression (D.LitDouble x)
    S.LitStr  x -> map (convLiteral . S.LitChar) x & foldr opCons (opNil D.TypeChar)
    S.LitChar x -> D.expression (D.LitChar x)
    S.LitBool x -> if x then opTrue else opFalse

instance Conv S.Literal where
    type Scalar S.Literal = D.Expression
    conv = return . convLiteral
