{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
 
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
import qualified Sodium.Nucleus.Program.Scalar as D

convert :: S.Program -> D.Program
convert = flip runReader [] . conv

type ConvEnv = [S.Name]

class Conv s d | s -> d where
	conv :: s -> Reader ConvEnv d

instance Conv S.Program D.Program where
	conv (S.Program funcs vars body) = do
		clMain <- do
			clBody <- conv (VB vars body)
			let clFuncSig = D.FuncSig D.NameMain M.empty D.TypeUnit
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

nameHook cs = do
	names <- ask
	let wrap = if cs `elem` names then D.NameUnique else id
	wrap <$> conv cs

instance Conv S.Func D.Func where
	conv (S.Func name params pasType vars body)
		= withReaderT (name:) $ do
			clFuncSig
				<-  D.FuncSig
				<$> conv name
				<*> (M.fromList <$> mapM conv (splitVarDecls params))
				<*> conv pasType
			clRetName <- nameHook name
			let enclose = D.bodyVars %~
				(M.insert clRetName $ clFuncSig ^. D.funcRetType)
			clBody <- enclose <$> conv (VB vars body)
			return $ D.Func clFuncSig clBody [D.Access clRetName]

instance Conv S.Name D.Name where
	conv = return . D.Name

splitVarDecls vardecls
	= [VarDecl name t | S.VarDecl names t <- vardecls, name <- names]

data VarDecl = VarDecl S.Name S.PasType

instance Conv VarDecl (D.Name, D.Type) where
	conv (VarDecl name pasType)
		 = (,) <$> conv name <*> conv pasType

instance Conv S.PasType D.Type where
	conv = \case
		S.PasInteger -> return D.TypeInteger
		S.PasLongInt -> return D.TypeInteger
		S.PasReal    -> return D.TypeDouble
		S.PasBoolean -> return D.TypeBoolean
		S.PasString  -> return D.TypeString
		S.PasType cs -> error "Custom types are not implemented"

binary op a b = D.Call op [a,b]

multifyIf expr bodyThen bodyElse = D.MultiIfBranch [(expr, bodyThen)] bodyElse

instance Conv S.Statement D.Statement where
	conv = \case
		S.BodyStatement body
			 -> D.BodyStatement
			<$> conv (VB [] body)
		S.Assign name expr -> D.Assign <$> nameHook name <*> conv expr
		S.Execute name exprs
			 -> D.Execute Nothing
			<$> case name of
				"readln"  -> return (D.OpReadLn undefined)
				"writeln" -> return D.OpPrintLn
				name -> D.OpName <$> conv name
			<*> mapM conv exprs
		S.ForCycle name fromExpr toExpr statement
			-> (D.ForStatement <$>)
			 $  D.ForCycle
			<$> nameHook name
			<*> (binary D.OpRange <$> conv fromExpr <*> conv toExpr)
			<*> convBodyStatement statement
		S.IfBranch expr bodyThen mBodyElse
			-> (D.MultiIfStatement <$>)
			 $  multifyIf
			<$> conv expr
			<*> convBodyStatement bodyThen
			<*> (maybeBodySingleton <$> mapM conv mBodyElse)
		S.CaseBranch expr leafs mBodyElse -> do
			(clCaseExpr, wrap) <- case expr of
				S.Access name -> (, id) <$> (D.Access <$> nameHook name)
				expr -> do
					clExpr <- conv expr
					let clName = D.Name "__CASE'__" -- generate a name?
					let clType = undefined -- typeof(expr)
					let wrap statement
						= D.BodyStatement
						$ D.Body
							(M.singleton clName clType)
							[D.Assign clName clExpr, statement]
					return (D.Access clName, wrap)
			let instRange = \case
				S.Binary S.OpRange exprFrom exprTo
					 -> (binary D.OpElem clCaseExpr)
					<$> (binary D.OpRange <$> conv exprFrom <*> conv exprTo)
				expr -> binary D.OpEquals clCaseExpr <$> conv expr
			let instLeaf (exprs, body)
				 =  (,)
				<$> (foldl1 (binary D.OpOr) <$> mapM instRange exprs)
				<*> convBodyStatement body
			let multiIfBranch
				 =  D.MultiIfBranch
				<$> mapM instLeaf leafs
				<*> (maybeBodySingleton <$> mapM conv mBodyElse)
			wrap <$> (D.MultiIfStatement <$> multiIfBranch)

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
		S.Access name -> D.Access <$> nameHook name
		S.Call name exprs
			 -> D.Call
			<$> (D.OpName <$> conv name)
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
		S.Unary op x -> case op of
			S.UOpPlus -> conv x
			S.UOpNegate
				 -> D.Call D.OpNegate
				<$> mapM conv [x]

instance Conv S.Operator D.Operator where
	conv = return . \case
		S.OpAdd -> D.OpAdd
		S.OpSubtract -> D.OpSubtract
		S.OpMultiply -> D.OpMultiply
		S.OpDivide -> D.OpDivide
		S.OpLess -> D.OpLess
		S.OpMore -> D.OpMore
		S.OpEquals -> D.OpEquals
		S.OpAnd -> D.OpAnd
		S.OpOr -> D.OpOr
		S.OpRange -> D.OpRange
