{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module Sodium.Haskell.Convert (convert) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Sodium.Nucleus.Program.Vector as S
import qualified Sodium.Haskell.Program as D
import qualified Language.Haskell.Exts as H

convert :: S.Program -> H.Module
convert = maybe (error "Sodium.Haskell.Convert") id . conv

class Conv s d | s -> d where
	conv :: s -> Maybe d

instance Conv S.Program H.Module where
	conv (S.Program funcs) = do
		funcDefs <- mapM conv funcs
		return $ D.program funcDefs
			(D.extensions ["LambdaCase", "TupleSections"])
			(map D.importDecl ["Control.Monad", "Control.Applicative"])

transformName :: S.Name -> D.Name
transformName = \case
	S.NameMain -> "main"
	S.Name cs
		-> (if reserved cs
			then ("_'"++)
			else id) cs
	S.NameGen u -> "_'" ++ show u
	S.NameUnique name -> transformName name ++ "'_"
	where reserved = flip elem
		[ "let"
		, "show"
		, "read"
		, "readLn"
		, "getLine"
		, "return"
		, "foldl"
		, "map"
		, "filter"
		, "undefined"
		, "main"
		, "import"
		, "_"
		]

data Name = Name S.Name S.Index
	deriving (Eq)

instance Conv Name D.Name where
	conv (Name name i) = case i of
		S.Index n -> return
			$ transformName name ++ genericReplicate n '\''
		S.Immutable -> return $ "const'" ++ transformName name
		S.Uninitialized -> return "undefined"

instance Conv S.ClType H.Type where
	conv = return . \case
		S.ClInteger -> D.hsType "Int"
		S.ClDouble  -> D.hsType "Double"
		S.ClBoolean -> D.hsType "Bool"
		S.ClString  -> D.hsType "String"
		S.ClVoid -> D.hsUnit

newtype Pure a = Pure a

instance Conv S.Body H.Exp where
	conv (S.Body _ statements resultExprs) = do
		hsStatements <- mapM conv statements
		hsRetValues <- mapM conv resultExprs
		let hsStatement
			= D.doExecute
			$ D.beta (D.access "return")
			$ D.expTuple hsRetValues
		return $ D.doexpr (hsStatements ++ [hsStatement])

instance Conv S.ForCycle H.Exp where
	conv (S.ForCycle argIndices argExprs name exprRange clBody) = do
		hsRange <- conv exprRange
		hsArgExpr <- D.expTuple <$> mapM conv argExprs
		hsFoldLambda <- conv (FoldLambda argIndices name) <*> conv clBody
		return $ betaL
			[ D.access "foldM"
			, hsFoldLambda
			, hsArgExpr
			, hsRange
			]

instance Conv S.MultiIfBranch H.Exp where
	conv (S.MultiIfBranch leafs bodyElse) = do
		let convLeaf (expr, body)
			 =  D.ifExpression
			<$> conv expr
			<*> conv body
		leafGens <- mapM convLeaf leafs
		hsBodyElse <- conv bodyElse
		return $ foldr ($) hsBodyElse leafGens

--TODO: Bind/Let inference
instance Conv S.Bind H.Stmt where
	conv (S.Bind retIndices (S.Execute (S.OpReadLn t) []))
		 =  D.doBind
		<$> (D.patTuple <$> conv (IndicesList retIndices))
		<*> if t == S.ClString
			then return $ D.access "getLine"
			else do
				hsType <- conv t
				return $ D.access "readLn" `D.typed` D.hsIO hsType
	conv (S.Bind [] (S.Execute S.OpPrintLn args))
		= case args of
			[S.Call S.OpShow [arg]]
				-> D.doExecute . D.beta (D.access "print") <$> conv arg
			args -> (<$> mapM conv args) $ \case
				[] -> D.doExecute
					$ D.beta (D.access "putStrLn") (D.primary (D.quote ""))
				hsExprs
					-> D.doExecute
					 $ D.beta (D.access "putStrLn")
					 $ foldl1 (\x y -> betaL [D.access "++", x, y])
					 $ hsExprs
	conv (S.Bind retIndices (S.Assign expr))
		 =  D.doLet
		<$> (D.patTuple <$> conv (IndicesList retIndices))
		<*> conv expr
	conv (S.Bind retIndices (S.ForStatement forCycle))
		 =  D.doBind
		<$> (D.patTuple <$> conv (IndicesList retIndices))
		<*> conv forCycle
	conv (S.Bind retIndices (S.MultiIfStatement multiIfBranch))
		 =  D.doBind
		<$> (D.patTuple <$> conv (IndicesList retIndices))
		<*> conv multiIfBranch
	conv (S.Bind retIndices (S.BodyStatement body))
		 =  D.doBind
		<$> (D.patTuple <$> conv (IndicesList retIndices))
		<*> conv body

instance Conv S.Func H.Decl where
	conv (S.Func (S.FuncSig S.NameMain params S.ClVoid) clBody) = do
		guard $ M.null params
		hsBody <- conv clBody
		return $ D.funcDef "main" [] hsBody
	conv (S.Func (S.FuncSig name params _) clBody)
		 = D.funcDef (transformName name) paramNames
		<$> conv (Pure clBody)
		where paramNames = map transformName (M.keys params)

data FoldLambda = FoldLambda S.IndicesList S.Name

instance Conv FoldLambda (H.Exp -> H.Exp) where
	conv (FoldLambda indices name) = do
		hsNames <- conv (IndicesList indices)
		hsName <- conv (Name name S.Immutable)
		return $ D.lambda [D.patTuple hsNames, D.patTuple [hsName]]

instance Conv (Pure S.Body) H.Exp where
	conv (Pure (S.Body _ statements resultExprs)) = msum
		[ do
			name1 <- case resultExprs of
				[S.Access name i] -> return $ Name name i
				_ -> mzero
			let appToLast f xs = case reverse xs of
				(x:xs') -> (, reverse xs') <$> f x
				_ -> mzero
			let extractIndex = \case
				[(name, i)] -> return (Name name i)
				_ -> mzero
			let convStatement (S.Bind indices statement)
				 =  (,)
				<$> extractIndex indices
				<*> case statement of
					S.Assign expr -> conv expr
					S.ForStatement forCycle -> conv (Pure forCycle)
					S.MultiIfStatement multiIfBranch -> conv (Pure multiIfBranch)
					S.BodyStatement body -> conv (Pure body)
					_ -> mzero
			((name2, hsExpr), statements) <- appToLast convStatement statements
			guard $ name1 == name2
			hsValueDefs <- mapM conv (map Pure statements)
			return $ pureLet hsValueDefs hsExpr
		, do
			hsValueDefs <- mapM conv (map Pure statements)
			hsRetValues <- mapM conv resultExprs
			return $ pureLet hsValueDefs (D.expTuple hsRetValues)
		]

instance Conv (Pure S.MultiIfBranch) H.Exp where
	conv (Pure (S.MultiIfBranch leafs bodyElse)) = do
		let convLeaf (expr, body)
			 =  D.ifExpression
			<$> conv expr
			<*> conv (Pure body)
		leafGens <- mapM convLeaf leafs
		hsBodyElse <- conv (Pure bodyElse)
		return $ foldr ($) hsBodyElse leafGens

instance Conv (Pure S.ForCycle) H.Exp where
	conv (Pure (S.ForCycle argIndices argExprs name exprRange clBody)) = do
		hsRange <- conv exprRange
		hsArgExpr <- D.expTuple <$> mapM conv argExprs
		hsFoldLambda
			<-  conv (FoldLambda argIndices name)
			<*> conv (Pure clBody)
		return $ betaL [D.access "foldl", hsFoldLambda, hsArgExpr, hsRange]


instance Conv (Pure S.Bind) H.Decl where
	conv (Pure (S.Bind retIndices statement))
		 =  D.valueDef
		<$> (D.patTuple <$> conv (IndicesList retIndices))
		<*> case statement of
			S.Assign expr -> conv expr
			S.ForStatement forCycle -> conv (Pure forCycle)
			S.MultiIfStatement multiIfBranch -> conv (Pure multiIfBranch)
			S.BodyStatement body -> conv (Pure body)
			_ -> mzero

betaL = foldl1 D.beta

pureLet [] = id
--pureLet defs = D.PureLet defs
pureLet _ = error "currently not supported"

newtype IndicesList = IndicesList S.IndicesList

instance Conv IndicesList [D.Name] where
	conv (IndicesList xs) = mapM (conv . uncurry Name) xs

instance Conv S.Expression H.Exp where
	conv (S.Primary prim) = return $ case prim of
		S.Quote cs -> D.primary (D.quote cs)
		S.INumber intSection -> D.primary (D.inumber intSection)
		S.FNumber intSection fracSection
			-> D.primary (D.fnumber intSection fracSection)
		S.ENumber intSection fracSection eSign eSection
			-> D.primary (D.enumber intSection fracSection eSign eSection)
		S.BTrue  -> D.access "True"
		S.BFalse -> D.access "False"
		S.Void   -> D.expTuple []
	conv (S.Access name i) = D.access <$> conv (Name name i)
	conv (S.Call op exprs) = do
		hsExprs <- mapM conv exprs
		return $ betaL (D.access (convOp op) : hsExprs)
	conv (S.Fold op exprs range) = do
		hsArgExpr <- D.expTuple <$> mapM conv exprs
		hsRange <- conv range
		return $ betaL [D.access "foldl", D.access (convOp op), hsArgExpr, hsRange]

convOp = \case
	S.OpNegate -> "negate"
	S.OpShow -> "show"
	S.OpProduct -> "product"
	S.OpSum -> "sum"
	S.OpAnd' -> "and"
	S.OpOr' -> "or"
	S.OpAdd -> "+"
	S.OpSubtract -> "-"
	S.OpMultiply -> "*"
	S.OpDivide -> "/"
	S.OpMore -> ">"
	S.OpLess -> "<"
	S.OpEquals -> "=="
	S.OpAnd -> "&&"
	S.OpOr -> "||"
	S.OpElem -> "elem"
	S.OpRange -> "enumFromTo"
	S.OpId -> "id"
	S.OpName name -> transformName name
