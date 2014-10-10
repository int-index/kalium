{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
 
module Sodium.Pascal.Convert (convert) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Monad.Identity hiding (mapM)
import qualified Data.Map  as M
import qualified Data.Char as C
import Data.Ratio
import Data.Traversable
import Control.Lens
-- S for Src, D for Dest
import qualified Sodium.Pascal.Program as S
import qualified Sodium.Nucleus.Program.Scalar as D

convert :: S.Program -> D.Program
convert = runIdentity . conv

nameV = D.Name ["v"]
nameF = D.Name ["f"]

class Conv s d | s -> d where
	conv :: s -> Identity d

instance Conv S.Program D.Program where
	conv (S.Program funcs vars body) = do
		clMain <- do
			clBody <- conv (VB vars body)
			let clFuncSig = D.FuncSig D.NameMain [] D.TypeUnit
			return $ D.Func clFuncSig clBody []
		clFuncs <- mapM conv funcs
		return $ D.Program (clMain:clFuncs)

convBodyStatement statement
	 =  review D.bodySingleton
	<$> conv statement

maybeBodySingleton
	= maybe D.bodyEmpty
	$ review D.bodySingleton

data VB = VB S.Vars S.Body

instance Conv VB D.Body where
	conv (VB vardecls statements)
		 =  D.Body
		<$> (M.fromList <$> mapM conv (splitVarDecls vardecls))
		<*> mapM conv statements

instance Conv S.Func D.Func where
    conv (S.Func name params pasType vars body)
        = do
            (retExprs, retType, enclose) <- case pasType of
                Nothing -> return ([], D.TypeUnit, id)
                Just ty -> do
                    let retName = nameV name
                    retType <- conv ty
                    let enclose = D.bodyVars %~ (M.insert retName retType)
                    return ([D.Access retName], retType, enclose)
            clFuncSig
                <-  D.FuncSig
                <$> return (nameF name)
                <*> mapM conv (splitParamDecls params)
                <*> return retType
            clBody <- enclose <$> conv (VB vars body)
            return $ D.Func clFuncSig clBody retExprs

splitVarDecls vardecls
    = [VarDecl name t | S.VarDecl names t <- vardecls, name <- names]

splitParamDecls paramdecls
    = [ParamDecl name r t | S.ParamDecl names r t <- paramdecls, name <- names]

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

multifyIf expr bodyThen bodyElse = D.MultiIf [(expr, bodyThen)] bodyElse

instance Conv S.Statement D.Statement where
	conv = \case
		S.BodyStatement body
			 -> D.BodyStatement
			<$> conv (VB [] body)
		S.Assign name expr -> D.Assign (nameV name) <$> conv expr
		S.Execute name exprs
			 -> D.Execute Nothing
			<$> case name of
				"readln"  -> return (D.NameOp $ D.OpReadLn undefined)
				"writeln" -> return (D.NameOp D.OpPrintLn)
				name -> return (nameF name)
			<*> mapM conv exprs
		S.ForCycle name fromExpr toExpr statement
			-> (D.ForStatement <$>)
			 $  D.ForCycle
			<$> return (nameV name)
			<*> (binary (D.NameOp D.OpRange) <$> conv fromExpr <*> conv toExpr)
			<*> convBodyStatement statement
		S.IfBranch expr bodyThen mBodyElse
			-> (D.MultiIfStatement <$>)
			 $  multifyIf
			<$> conv expr
			<*> convBodyStatement bodyThen
			<*> (maybeBodySingleton <$> mapM conv mBodyElse)
		S.CaseBranch expr leafs mBodyElse -> do
			(clCaseExpr, wrap) <- case expr of
				S.Access name -> return (D.Access (nameV name), id)
				expr -> do
					clExpr <- conv expr
					let clName = D.Name ["g"] "case1" -- generate a name?
					let clType = D.TypeUnit -- typeof(expr)
					let wrap statement
						= D.BodyStatement
						$ D.Body
							(M.singleton clName clType)
							[D.Assign clName clExpr, statement]
					return (D.Access clName, wrap)
			let instRange = \case
				S.Binary S.OpRange exprFrom exprTo
					 -> (binary (D.NameOp D.OpElem) clCaseExpr)
					<$> (binary (D.NameOp D.OpRange) <$> conv exprFrom <*> conv exprTo)
				expr -> binary (D.NameOp D.OpEquals) clCaseExpr <$> conv expr
			let instLeaf (exprs, body)
				 =  (,)
				<$> (foldl1 (binary (D.NameOp D.OpOr)) <$> mapM instRange exprs)
				<*> convBodyStatement body
			let multiIf
				 =  D.MultiIf
				<$> mapM instLeaf leafs
				<*> (maybeBodySingleton <$> mapM conv mBodyElse)
			wrap <$> (D.MultiIfStatement <$> multiIf)

inumber intSection = D.LitInteger $ parseInt intSection
fnumber intSection fracSection = D.LitDouble $ parseFrac intSection fracSection
enumber intSection fracSection eSign eSection
    = D.LitDouble $ parseExp intSection fracSection eSign eSection

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
		S.Access name -> return $ D.Access (nameV name)
		S.Call name exprs
			 -> D.Call
			<$> pure (nameF name)
			<*> mapM conv exprs
		S.INumber intSection -> return (D.Primary $ inumber intSection)
		S.FNumber intSection fracSection
			-> return (D.Primary $ fnumber intSection fracSection)
		S.ENumber intSection fracSection eSign eSection
			-> return (D.Primary $ enumber intSection fracSection eSign eSection)
		S.Quote cs -> return $ D.Primary (D.LitString  cs)
		S.BTrue  -> return $ D.Primary (D.LitBoolean True)
		S.BFalse -> return $ D.Primary (D.LitBoolean False)
		S.Binary op x y -> binary <$> conv op <*> conv x <*> conv y
		S.Unary op x -> D.Call <$> conv op <*> mapM conv [x]

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
