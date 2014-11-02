{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
 
module Sodium.Pascal.Convert (convert) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Monad.Reader hiding (mapM)
import qualified Data.Map  as M
import Data.Traversable
-- S for Src, D for Dest
import qualified Sodium.Pascal.Program as S
import qualified Sodium.Nucleus.Scalar.Program as D
import qualified Sodium.Nucleus.Scalar.Build   as D
import Sodium.Nucleus.Name

convert :: NameStack t m => S.Program -> m (D.Program D.Expression)
convert program = runReaderT (conv program) M.empty

nameV = D.NameSpace "v" . D.Name
nameF = D.NameSpace "f" . D.Name

class Conv s d | s -> d where
    conv :: NameStack t m => s -> ReaderT (M.Map S.Name S.Type) m d

instance Conv S.Program (D.Program D.Expression) where
    conv (S.Program funcs vars body) = do
        clMain <- do
            clBody <- convScope vars $ D.Body <$> conv body <*> pure (D.atom ())
            let noparams = D.Scope ([] :: D.Params)
            return $ D.Func D.TypeUnit (noparams clBody)
        clFuncs <- mapM conv funcs
        return $ D.Program (M.fromList $ (D.NameMain, clMain):clFuncs)

convScope vardecls inner
        = D.Scope
       <$> (D.scoping <$> mapM conv (M.toList vardecls))
       <*> local (M.union vardecls) inner

convScope' paramdecls inner
        = D.Scope
       <$> mapM conv paramdecls
       <*> local (M.union (M.fromList $ map paramDeclToTup paramdecls)) inner

instance Conv S.Body (D.Statement D.Expression) where
    conv statements = D.Group <$> mapM conv statements

instance Conv S.Func (D.Name, D.Func D.Expression) where
    conv (S.Func name params pasType vars body) = do
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

paramDeclToTup (S.ParamDecl name (_, ty)) = (name, ty)

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

convReadLn [S.Access name] = do
    ty <- lookupType name
    clType <- conv ty
    return $ D.Exec
        (Just $ nameV name)
        (D.NameOp $ D.OpReadLn clType)
        []
convReadLn _ = error "IOMagic supports only single-value read operations"

convWriteLn exprs = do
    let convArg expr = do
          -- TODO: apply `show` only to non-String
          -- expressions as soon as typecheck is implemented
          noShow <- case expr of
            S.Primary (S.LitStr _) -> return True
            S.Access name -> do
                ty <- lookupType name
                return (ty == S.TypeString || ty == S.TypeChar)
            _ -> return False
          let wrap = if noShow then id else (\e -> D.Call (D.NameOp D.OpShow) [e])
          wrap <$> conv expr
    D.Exec Nothing (D.NameOp D.OpPrintLn) <$> mapM convArg exprs


lookupType name = do
    mtype <- asks (M.lookup name)
    maybe (error "IOMagic lookup error") return mtype

instance Conv S.Statement (D.Statement D.Expression) where
    conv = \case
        S.BodyStatement body -> D.statement <$> conv body
        S.Assign name expr -> D.assign (nameV name) <$> conv expr
        S.Execute "readln"  exprs -> D.statement <$> convReadLn  exprs
        S.Execute "writeln" exprs -> D.statement <$> convWriteLn exprs
        S.Execute name exprs
             -> fmap D.statement
             $  D.Exec Nothing (nameF name)
            <$> mapM conv exprs
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
            <*> (D.statements <$> mapM conv mBodyElse)
        S.CaseBranch expr leafs mBodyElse -> do
            clExpr <- conv expr
            clName <- namepop
            let clType = D.TypeUnit -- typeof(expr)
            let clCaseExpr = D.expression clName
            let instRange = \case
                    S.Binary S.OpRange exprFrom exprTo
                         -> (binary (D.NameOp D.OpElem) clCaseExpr)
                        <$> (binary (D.NameOp D.OpRange) <$> conv exprFrom <*> conv exprTo)
                    expr -> binary (D.NameOp D.OpEquals) clCaseExpr <$> conv expr
            let instLeaf (exprs, body)
                     =  (,)
                    <$> (foldl1 (binary (D.NameOp D.OpOr)) <$> mapM instRange exprs)
                    <*> conv body
            leafs <- mapM instLeaf leafs
            leafElse <- D.statements <$> mapM conv mBodyElse
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
        S.Call name exprs -> D.Call <$> pure (nameF name) <*> mapM conv exprs
        S.Primary lit -> conv lit
        S.Binary op x y -> binary <$> conv op <*> conv x <*> conv y
        S.Unary  op x   -> D.Call <$> conv op <*> mapM conv [x]

instance Conv S.Literal D.Expression where
    conv = \case
        S.LitInt  x -> return (D.expression x)
        S.LitReal x -> return (D.expression x)
        S.LitStr  x -> return (D.expression x)
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
        S.OpNot -> D.OpNot
        S.OpXor -> D.OpXor
        S.OpRange -> D.OpRange

instance Conv S.UnaryOperator D.Name where
    conv = return . D.NameOp . \case
        S.UOpPlus   -> D.OpId
        S.UOpNegate -> D.OpNegate
        S.UOpNot    -> D.OpNot
