{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
 
module Sodium.Pascal.Convert (convert, TypeError(..)) where

import qualified Data.Map as M
import Data.Traversable

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
-- S for Src, D for Dest
import qualified Sodium.Pascal.Program as S
import qualified Sodium.Nucleus.Scalar.Program as D
import qualified Sodium.Nucleus.Scalar.Build   as D
import Sodium.Nucleus.Name

declareLenses [d|

    data TypeScope = TypeScope
        { tsFunctions :: M.Map S.Name S.FuncSig
        , tsVariables :: M.Map S.Name S.Type
        } deriving (Eq)

                |]

data TypeError
    = NoAccess
    | NoFunction
    | TypeVoid
    deriving (Eq, Show)

convert :: (NameStack t m, MonadError TypeError m) => S.Program -> m (D.Program D.Expression)
convert program = runReaderT (conv program) (TypeScope M.empty M.empty)

nameV = D.NameSpace "v" . D.Name
nameF = D.NameSpace "f" . D.Name

class Conv s d | s -> d where
    conv :: (NameStack t m, MonadError TypeError m) => s -> ReaderT TypeScope m d

instance Conv S.Program (D.Program D.Expression) where
    conv (S.Program funcs vars body)
        = local (tsFunctions %~ M.union funcSigs) $ do
            clMain <- do
                clBody <- convScope vars
                    $ D.Body <$> conv body <*> pure (D.atom ())
                let noparams = D.Scope ([] :: D.Params)
                return $ D.Func D.TypeUnit (noparams clBody)
            clFuncs <- traverse conv funcs
            return $ D.Program (M.fromList $ (D.NameMain, clMain):clFuncs)
        where funcSigs = M.unions (map funcSigOf funcs)
              funcSigOf (S.Func name funcSig _ _) = M.singleton name funcSig

convScope vardecls inner
        = D.Scope
       <$> (D.scoping <$> traverse conv (M.toList vardecls))
       <*> local (tsVariables %~ M.union vardecls) inner

convScope' paramdecls inner
        = D.Scope
       <$> traverse conv paramdecls
       <*> local (tsVariables %~ M.union vardecls) inner
       where paramDeclToTup (S.ParamDecl name (_, ty)) = (name, ty)
             vardecls = M.fromList $ map paramDeclToTup paramdecls

instance Conv S.Body (D.Statement D.Expression) where
    conv statements = D.Group <$> traverse conv statements

instance Conv S.Func (D.Name, D.Func D.Expression) where
    conv (S.Func name (S.FuncSig params pasType) vars body) = do
        (retExpr, retType, retVars) <- case pasType of
            Nothing -> return (D.atom (), D.TypeUnit, M.empty)
            Just ty -> do
                let retName = nameV name
                retType <- conv ty
                return (D.atom retName, retType, M.singleton name ty)
        clScope <- convScope' params
                 $ convScope (M.union vars retVars)
                 $ D.Body <$> conv body <*> pure retExpr
        let fname = nameF name
        return $ (fname, D.Func retType clScope)

instance Conv (S.Name, S.Type) (D.Name, D.Type) where
    conv (name, pasType)
         = (,) <$> pure (nameV name) <*> conv pasType

instance Conv S.ParamDecl (D.Name, D.ByType) where
    conv (S.ParamDecl name (r, pasType))
        = (,) <$> pure (nameV name) <*> ((,) <$> conv r <*> conv pasType)

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

binary op a b = D.Call op [a,b]

convReadLn [e@(S.Access name')] = do
    let name = nameV name'
    typecheck e >>= \case
        S.TypeString -> return $ D.Exec (Just name) (D.NameOp D.OpGetLn) []
        ty -> do
            clType <- conv ty
            return $ D.Exec (Just name) (D.NameOp $ D.OpReadLn clType) []
convReadLn _ = error "IOMagic supports only single-value read operations"

convWriteLn exprs = do
    let convArg expr = do
          ty <- typecheck expr
          let wrap = case ty of
                S.TypeString -> id
                S.TypeChar -> \e -> D.Call (D.NameOp D.OpSingleton) [e]
                _ -> \e -> D.Call (D.NameOp D.OpShow) [e]
          wrap <$> conv expr
    D.Exec Nothing (D.NameOp D.OpPrintLn) <$> traverse convArg exprs

typecheck :: (MonadReader TypeScope m, MonadError TypeError m)
          => S.Expression -> m S.Type
typecheck = \case
    S.Primary lit -> return $ case lit of
        S.LitBool _ -> S.TypeBoolean
        S.LitInt  _ -> S.TypeInteger
        S.LitReal _ -> S.TypeReal
        S.LitChar _ -> S.TypeChar
        S.LitStr  _ -> S.TypeString
    S.Access name -> do
        mtype <- asks (M.lookup name . view tsVariables)
        maybe (throwError NoAccess) return mtype
    S.Call (Right name) _ -> do
        mfuncsig <- asks (M.lookup name . view tsFunctions)
        case mfuncsig of
            Nothing -> throwError NoFunction
            Just (S.FuncSig _ mtype) -> case mtype of
                Nothing -> throwError TypeVoid
                Just t -> return t
    S.Call (Left _op) _ -> return (S.TypeCustom "not-implemented")

instance Conv S.Statement (D.Statement D.Expression) where
    conv = \case
        S.BodyStatement body -> D.statement <$> conv body
        S.Assign name expr -> D.assign (nameV name) <$> conv expr
        S.Execute "readln"  exprs -> D.statement <$> convReadLn  exprs
        S.Execute "writeln" exprs -> D.statement <$> convWriteLn exprs
        S.Execute name exprs
             -> fmap D.statement
             $  D.Exec Nothing (nameF name)
            <$> traverse conv exprs
        S.ForCycle name fromExpr toExpr statement -> do
            let clName = nameV name
            clFromExpr <- conv fromExpr
            clToExpr   <- conv toExpr
            let clRange = binary (D.NameOp D.OpRange) clFromExpr clToExpr
            clAction <- conv statement
            let clForCycle = D.statement (D.ForCycle clName clRange clAction)
            return $ D.Group [clForCycle, D.assign clName clToExpr]
        S.IfBranch expr bodyThen mBodyElse
             -> fmap D.statement
             $  D.If
            <$> conv expr
            <*> conv bodyThen
            <*> (D.statements <$> traverse conv mBodyElse)
        S.CaseBranch expr leafs mBodyElse -> do
            clType <- typecheck expr >>= conv
            clExpr <- conv expr
            clName <- namepop
            let clCaseExpr = D.expression clName
            let instRange = \case
                    S.Call (Left S.OpRange) [exprFrom, exprTo]
                         -> (binary (D.NameOp D.OpElem) clCaseExpr)
                        <$> (binary (D.NameOp D.OpRange) <$> conv exprFrom <*> conv exprTo)
                    expr -> binary (D.NameOp D.OpEquals) clCaseExpr <$> conv expr
            let instLeaf (exprs, body)
                     =  (,)
                    <$> (foldl1 (binary (D.NameOp D.OpOr)) <$> traverse instRange exprs)
                    <*> conv body
            leafs <- traverse instLeaf leafs
            leafElse <- D.statements <$> traverse conv mBodyElse
            let statement = foldr
                    (\(cond, ifThen) ifElse ->
                        D.statement $ D.If cond ifThen ifElse)
                     leafElse leafs
            return $ D.statement $ D.Scope
                        (M.singleton clName clType)
                        (D.Group [D.assign clName clExpr, statement])

instance Conv S.Expression D.Expression where
    conv = \case
        S.Access name -> return $ D.expression (nameV name)
        S.Call (Left op)    exprs -> D.Call <$> conv op           <*> traverse conv exprs
        S.Call (Right name) exprs -> D.Call <$> pure (nameF name) <*> traverse conv exprs
        S.Primary lit -> conv lit

instance Conv S.Literal D.Expression where
    conv = \case
        S.LitInt  x -> return (D.expression x)
        S.LitReal x -> return (D.expression x)
        S.LitStr  x -> return (D.expression x)
        S.LitChar x -> return (D.expression x)
        S.LitBool x -> return (D.expression x)

instance Conv S.Operator D.Name where
    conv = return . D.NameOp . \case
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
        S.OpRange -> D.OpRange
        S.OpPlus   -> D.OpId
        S.OpNegate -> D.OpNegate
        S.OpNot    -> D.OpNot
