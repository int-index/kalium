{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
 
module Sodium.Pascal.Convert (convert) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Monad.Reader hiding (mapM)
import qualified Data.Map  as M
import qualified Data.Char as C
import Data.Ratio
import Data.Traversable
import Control.Lens
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
	conv :: NameStack t m => s -> ReaderT (M.Map S.Name S.PasType) m d

instance Conv S.Program (D.Program D.Expression) where
	conv (S.Program funcs vars body) = do
		clMain <- do
			clBody <- conv (VB vars body)
			let clFuncSig = D.FuncSig D.NameMain [] D.TypeUnit
			return $ D.Func clFuncSig [] clBody (D.atom ())
		clFuncs <- mapM conv funcs
		return $ D.Program (clMain:clFuncs)

maybeStatement = maybe (D.Group []) id

data VB = VB S.Vars S.Body

instance Conv VB (D.Scope D.Expression) where
    conv (VB vardecls statements)
        = D.Scope
       <$> (M.fromList <$> mapM conv varDecls)
       <*> local (M.union (M.fromList $ map varDeclToTup varDecls))
            (D.Group <$> mapM conv statements)
       where varDecls = splitVarDecls vardecls

instance Conv S.Func (D.Func D.Expression) where
    conv (S.Func name params pasType vars body)
        = do
            (retExpr, retType, retVars) <- case pasType of
                Nothing -> return (D.atom (), D.TypeUnit, [])
                Just ty -> do
                    let retName = nameV name
                    retType <- conv ty
                    return (D.atom retName, retType, [S.VarDecl [name] ty])
            let paramDecls = splitParamDecls params
            (clParams, clParamTypes) <- unzip <$> mapM conv paramDecls
            let clFuncSig = D.FuncSig (nameF name) clParamTypes retType
            clScope <- local (M.union (M.fromList $ map paramDeclToTup paramDecls))
                    $ conv (VB (vars ++ retVars) body)
            return $ D.Func clFuncSig clParams clScope retExpr

splitVarDecls vardecls
    = [VarDecl name t | S.VarDecl names t <- vardecls, name <- names]

splitParamDecls paramdecls
    = [ParamDecl name r t | S.ParamDecl names r t <- paramdecls, name <- names]

varDeclToTup (VarDecl name ty) = (name, ty)
paramDeclToTup (ParamDecl name _ ty) = (name, ty)

data VarDecl   = VarDecl   S.Name      S.PasType
data ParamDecl = ParamDecl S.Name Bool S.PasType

instance Conv VarDecl (D.Name, D.Type) where
    conv (VarDecl name pasType)
         = (,) <$> pure (nameV name) <*> conv pasType

instance Conv ParamDecl (D.Name, D.ByType) where
    conv (ParamDecl name r pasType)
        = (,) <$> pure (nameV name) <*> (annotate <$> conv pasType)
        where annotate = (,) (if r then D.ByReference else D.ByValue)

instance Conv S.PasType D.Type where
	conv = \case
		S.PasInteger -> return D.TypeInteger
		S.PasLongInt -> return D.TypeInteger
		S.PasReal    -> return D.TypeDouble
		S.PasBoolean -> return D.TypeBoolean
		S.PasString  -> return D.TypeString
		S.PasType _  -> error "Custom types are not implemented"

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
            S.Quote _ -> return True
            S.Access name -> do
                ty <- lookupType name
                return (ty == S.PasString)
            _ -> return False
          let wrap = if noShow then id else (\e -> D.Call (D.NameOp D.OpShow) [e])
          wrap <$> conv expr
    D.Exec Nothing (D.NameOp D.OpPrintLn) <$> mapM convArg exprs


lookupType name = do
    mtype <- asks (M.lookup name)
    maybe (error "IOMagic lookup error") return mtype

instance Conv S.Statement (D.Statement D.Expression) where
    conv = \case
        S.BodyStatement body -> D.statement <$> conv (VB [] body)
        S.Assign name expr -> D.assign (nameV name) <$> conv expr
        S.Execute "readln"  exprs -> D.statement <$> convReadLn  exprs
        S.Execute "writeln" exprs -> D.statement <$> convWriteLn exprs
        S.Execute name exprs
             -> fmap D.statement
             $  D.Exec Nothing (nameF name)
            <$> mapM conv exprs
        S.ForCycle name fromExpr toExpr statement
             -> fmap D.statement
             $  D.ForCycle
            <$> return (nameV name)
            <*> (binary (D.NameOp D.OpRange) <$> conv fromExpr <*> conv toExpr)
            <*> conv statement
        S.IfBranch expr bodyThen mBodyElse
             -> fmap D.statement
             $  D.If
            <$> conv expr
            <*> conv bodyThen
            <*> (maybeStatement <$> mapM conv mBodyElse)
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
            leafElse <- maybeStatement <$> mapM conv mBodyElse
            let statement = foldr
                    (\(cond, ifThen) ifElse ->
                        D.statement $ D.If cond ifThen ifElse)
                     leafElse leafs
            return $ D.statement $ D.Scope
                        (M.singleton clName clType)
                        (D.Group [D.assign clName clExpr, statement])

parseInt :: String -> Integer
parseInt = foldl (\acc c -> fromIntegral (C.digitToInt c) + acc * 10) 0

parseFrac :: String -> String -> Rational
parseFrac intSection fracSection = parseInt (intSection ++ fracSection)
                                 % 10 ^ length fracSection

parseExp :: String -> String -> Bool -> String -> Rational
parseExp intSection fracSection eSign eSection
    = (if eSign then (*) else (/))
        (parseFrac intSection fracSection)
        (10 ^ parseInt eSection)

instance Conv S.Expression D.Expression where
    conv = \case
        S.Access name -> return $ D.expression (nameV name)
        S.Call name exprs -> D.Call <$> pure (nameF name) <*> mapM conv exprs
        S.INumber intSection -> return $ D.expression (parseInt intSection)
        S.FNumber intSection fracSection
            -> return $ D.expression (parseFrac intSection fracSection)
        S.ENumber intSection fracSection eSign eSection
            -> return $ D.expression (parseExp intSection fracSection eSign eSection)
        S.Quote cs -> return (D.expression cs)
        S.BTrue    -> return (D.expression True)
        S.BFalse   -> return (D.expression False)
        S.Binary op x y -> binary <$> conv op <*> conv x <*> conv y
        S.Unary  op x   -> D.Call <$> conv op <*> mapM conv [x]

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
