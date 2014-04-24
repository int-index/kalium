{-# LANGUAGE FlexibleInstances #-}
module Sodium.Haskell.Convert (convert) where

import Data.List (genericReplicate)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
-- S for Src, D for Dest
import qualified Sodium.Chloride.Program.Vector as S
import qualified Sodium.Haskell.Program as D

convert :: S.Program -> D.Program
convert = maybe (error "Sodium.Haskell.Convert") id . conv

class Conv s d | s -> d where
	conv :: s -> Maybe d

instance Conv S.Program D.Program where
	conv (S.Program funcs) = do
		funcDefs <- mapM conv funcs
		return $ D.Program (map D.Def funcDefs)
			[ "Control.Monad"
			, "Control.Applicative"
			]
			[ "LambdaCase"
			, "TupleSections"
			]

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

instance Conv S.ClType D.HsType where
	conv = return . \case
		S.ClInteger -> D.HsType "Int"
		S.ClDouble  -> D.HsType "Double"
		S.ClBoolean -> D.HsType "Bool"
		S.ClString  -> D.HsType "String"
		S.ClVoid -> D.HsUnit

newtype Pure a = Pure a

instance Conv S.Body D.Expression where
	conv (S.Body _ statements resultExprs) = do
		hsStatements <- mapM conv statements
		hsRetValues <- mapM conv resultExprs
		let hsStatement
			= D.DoExecute
			$ D.Beta (D.Access "return")
			$ D.Tuple hsRetValues
		return $ D.DoExpression (hsStatements ++ [hsStatement])

instance Conv S.ForCycle D.Expression where
	conv (S.ForCycle argIndices argExprs name exprRange clBody) = do
		hsRange <- conv exprRange
		hsArgExpr <- D.Tuple <$> mapM conv argExprs
		hsFoldLambda <- conv (FoldLambda argIndices name) <*> conv clBody
		return $ beta
			[ D.Access "foldM"
			, hsFoldLambda
			, hsArgExpr
			, hsRange
			]

instance Conv S.MultiIfBranch D.Expression where
	conv (S.MultiIfBranch leafs bodyElse) = do
		let convLeaf (expr, body)
			 =  D.IfExpression
			<$> conv expr
			<*> conv body
		leafGens <- mapM convLeaf leafs
		hsBodyElse <- conv bodyElse
		return $ foldr ($) hsBodyElse leafGens

--TODO: Bind/Let inference
instance Conv (S.IndicesList, S.Statement) D.DoStatement where
	conv (retIndices, S.Execute (S.OpReadLn t) [])
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> if t == S.ClString
			then return $ D.Access "getLine"
			else do
				hsType <- conv t
				return $ D.Access "readLn" `D.Typed` D.HsIO hsType
	conv (retIndices, S.Execute S.OpPrintLn args)
		| null retIndices
		= case args of
			[S.Call S.OpShow [arg]]
				-> D.DoExecute . D.Beta (D.Access "print") <$> conv arg
			args -> (<$> mapM conv args) $ \case
				[] -> D.DoExecute
					$ D.Beta (D.Access "putStrLn") (D.Primary (D.Quote ""))
				hsExprs
					-> D.DoExecute
					 $ D.Beta (D.Access "putStrLn")
					 $ foldl1 (\x y -> beta [D.Access "++", x, y])
					 $ hsExprs
	conv (retIndices, S.Assign expr)
		 =  D.DoLet
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv expr
	conv (retIndices, S.ForStatement forCycle)
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv forCycle
	conv (retIndices, S.MultiIfStatement multiIfBranch)
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv multiIfBranch
	conv (retIndices, S.BodyStatement body)
		 =  D.DoBind
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> conv body

instance Conv S.Func D.ValueDef where
	conv (S.Func (S.FuncSig S.NameMain params S.ClVoid) clBody) = do
		guard $ M.null params
		hsBody <- conv clBody
		return $ D.ValueDef (D.PatFunc "main" []) hsBody
	conv (S.Func (S.FuncSig name params _) clBody)
		 =  D.ValueDef (D.PatFunc (transformName name) paramNames)
		<$> conv (Pure clBody)
		where paramNames = map transformName (M.keys params)

data FoldLambda = FoldLambda S.IndicesList S.Name

instance Conv FoldLambda (D.Expression -> D.Expression) where
	conv (FoldLambda indices name) = do
		hsNames <- conv (IndicesList indices)
		hsName <- conv (Name name S.Immutable)
		return $ D.Lambda [D.PatTuple hsNames, D.PatTuple [hsName]]

instance Conv (Pure S.Body) D.Expression where
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
			let convStatement (indices, statement)
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
			return $ pureLet hsValueDefs (D.Tuple hsRetValues)
		]

instance Conv (Pure S.MultiIfBranch) D.Expression where
	conv (Pure (S.MultiIfBranch leafs bodyElse)) = do
		let convLeaf (expr, body)
			 =  D.IfExpression
			<$> conv expr
			<*> conv (Pure body)
		leafGens <- mapM convLeaf leafs
		hsBodyElse <- conv (Pure bodyElse)
		return $ foldr ($) hsBodyElse leafGens

instance Conv (Pure S.ForCycle) D.Expression where
	conv (Pure (S.ForCycle argIndices argExprs name exprRange clBody)) = do
		hsRange <- conv exprRange
		hsArgExpr <- D.Tuple <$> mapM conv argExprs
		hsFoldLambda
			<-  conv (FoldLambda argIndices name)
			<*> conv (Pure clBody)
		return $ beta [D.Access "foldl", hsFoldLambda, hsArgExpr, hsRange]


instance Conv (Pure (S.IndicesList, S.Statement)) D.ValueDef where
	conv (Pure (retIndices, statement))
		 =  D.ValueDef
		<$> (D.PatTuple <$> conv (IndicesList retIndices))
		<*> case statement of
			S.Assign expr -> conv expr
			S.ForStatement forCycle -> conv (Pure forCycle)
			S.MultiIfStatement multiIfBranch -> conv (Pure multiIfBranch)
			S.BodyStatement body -> conv (Pure body)
			_ -> mzero

beta = foldl1 D.Beta

pureLet [] = id
pureLet defs = D.PureLet defs

newtype IndicesList = IndicesList S.IndicesList

instance Conv IndicesList [D.Name] where
	conv (IndicesList xs) = mapM (conv . uncurry Name) xs

instance Conv S.Expression D.Expression where
	conv (S.Primary prim) = return $ case prim of
		S.Quote cs -> D.Primary (D.Quote cs)
		S.INumber intSection -> D.Primary (D.INumber intSection)
		S.FNumber intSection fracSection
			-> D.Primary (D.FNumber intSection fracSection)
		S.ENumber intSection fracSection eSign eSection
			-> D.Primary (D.ENumber intSection fracSection eSign eSection)
		S.BTrue  -> D.Access "True"
		S.BFalse -> D.Access "False"
		S.Void   -> D.Tuple []
	conv (S.Access name i) = D.Access <$> conv (Name name i)
	conv (S.Call op exprs) = do
		hsExprs <- mapM conv exprs
		return $ beta (D.Access (convOp op) : hsExprs)
	conv (S.Fold op exprs range) = do
		hsArgExpr <- D.Tuple <$> mapM conv exprs
		hsRange <- conv range
		return $ beta [D.Access "foldl", D.Access (convOp op), hsArgExpr, hsRange]

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
