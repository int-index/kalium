module Sodium.Pascal.Parse (parse) where

import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Sodium.Tr (head, before, fallback, expect)
import qualified Sodium.Pascal.Tokenize as T
import Sodium.Pascal.Program

parse :: String -> Program
parse cs
	= either error id
	$ maybe abort verify (runStateT programTr xs)
	where
		(csBad, xs) = T.tokenize cs
		isDot (T.Dot:_) = True
		isDot _ = False
		verify (program, xs)
			| isDot xs  = Right program
			| otherwise = abort
		abort
			| null csBad = Left "Sodium.Pascal.Parse"
			| otherwise  = Left "Sodium.Pascal.Tokenize"

-- Useful combinators

sepr elemTr opTr = tr where
	tr = elemTr >>= next
	next a = fallback a $ opTr <*> return a <*> tr

sepl elemTr opTr = elemTr >>= next where
	next a = fallback a $ (opTr <*> return a <*> elemTr) >>= next


-- Syntactic definitions

programTr
	 =  Program
	<$> many funcTr
	<*> varsTr
	<*> bodyTr

varsTr
	 =  fallback []
	 $  expect T.KwVar
	 *> many (varDeclTr
	<*  expect T.Semicolon)

varDeclTr
	 =  VarDecl
	<$> varNamesTr
	<*> typeTr

varNamesTr = sepb T.Comma T.Colon nameTr

typeTr = nameTr >>= \case
	"integer" -> return PasInteger
	"longint" -> return PasLongInt
	"real" -> return PasReal
	"boolean" -> return PasBoolean
	"string"  -> return PasString
	"array" -> do
		expect T.KwOf
		PasArray <$> typeTr
	cs -> return $ PasType cs

bodyTr
	 = expect T.KwBegin
	*> sepb T.Semicolon T.KwEnd statementTr

funcTr
	 =  Func
	<$  expect T.KwFunction
	<*> nameTr
	<*> fallback [] paramsTr
	<*  expect T.Colon
	<*> typeTr
	<*  expect T.Semicolon
	<*> varsTr
	<*> bodyTr
	<*  expect T.Semicolon

paramsTr
	 =  expect T.LParen
	 *> sepb T.Semicolon T.RParen varDeclTr

statementTr
	= msum
	[ assignTr
	, executeTr
	, forCycleTr
	, ifBranchTr
	, caseBranchTr
	, BodyStatement <$> bodyTr
	, return $ BodyStatement []
	]

assignTr
	 =  Assign
	<$> nameTr
	<*  expect T.Assign
	<*> conditionTr

executeTr
	 =  Execute
	<$> nameTr
	<*> fallback [] argsTr

forCycleTr
	 = ForCycle
	<$  expect T.KwFor
	<*> nameTr
	<*  expect T.Assign
	<*> conditionTr
	<*  expect T.KwTo
	<*> conditionTr
	<*  expect T.KwDo
	<*> statementTr

ifBranchTr
	 =  IfBranch
	<$  expect T.KwIf
	<*> conditionTr
	<*> thenClause
	<*> optional elseClause
	where
		thenClause
			 = expect T.KwThen
			*> statementTr
		elseClause
			 = expect T.KwElse
			*> statementTr

caseBranchTr
	 =  CaseBranch
	<$  expect T.KwCase
	<*> conditionTr
	<*  expect T.KwOf
	<*> many (caseClause
	<*  expect T.Semicolon)
	<*> optional (elseClause
	<*  expect T.Semicolon)
	<*  expect T.KwEnd
	where
		caseClause
			 =  (,)
			<$> sepb T.Comma T.Colon rangeTr
			<*> statementTr
		elseClause
			 =  expect T.KwElse
			 *> statementTr

sodiumTr
	=  expect T.SodiumSpecial
	*> (snd <$> before nameTr (expect T.RBrace))

sepb septok endtok elemTr = end <|> next where
	end = expect endtok *> return []
	next
		 =  (:)
		<$> elemTr
		<*> (expect septok *> next <|> end)

sepn elem1Tr elem2Tr opTr = do
	elem1 <- elem1Tr
	mOpf <- optional opfTr
	return $ fromMaybe id mOpf elem1
	where
		opfTr = do
			op <- opTr
			elem2 <- elem2Tr
			return $ flip (Binary op) elem2

conditionTr
	= sepn expressionTr expressionTr
	$ head >>= \case
		T.Suck -> return OpLess
		T.Blow -> return OpMore
		T.EqSign -> return OpEquals
		_ -> mzero

rangeTr
	= sepn expressionTr expressionTr
	$ head >>= \case
		T.DoubleDot -> return OpRange
		_ -> mzero

expressionTr = sepl termTr $
	head >>= \case
		T.Plus  -> return (Binary OpAdd)
		T.Minus -> return (Binary OpSubtract)
		T.KwOr  -> return (Binary OpOr)
		_ -> mzero

termTr = sepl primTr $
	head >>= \case
		T.Asterisk -> return (Binary OpMultiply)
		T.Slash -> return (Binary OpDivide)
		T.KwAnd -> return (Binary OpAnd)
		_ -> mzero

unaryTr = fallback id opTr where
	opTr = head >>= \case
		T.Plus -> return (Unary UOpPlus)
		T.Minus -> return (Unary UOpNegate)
		_ -> mzero

primTr
	 =  unaryTr
	<*> msum
	[ Call <$> nameTr <*> argsTr
	, accessTr
	, numberTr
	, quoteTr
	, boolTr
	, enclosedTr
	]

enclosedTr
	=  expect T.LParen
	*> conditionTr
	<* expect T.RParen

argsTr
	=  expect T.LParen
	*> sepb T.Comma T.RParen conditionTr

accessTr
	 =  Access
	<$> nameTr

numberTr = head >>= \case
	T.INumber intSection -> return $
		INumber intSection
	T.FNumber intSection fracSection -> return $
		FNumber intSection fracSection
	T.ENumber intSection fracSection eSign eSection -> return $
		ENumber intSection fracSection eSign eSection
	_ -> mzero

quoteTr = head >>= \case
	T.Quote cs -> return $ Quote cs
	_ -> mzero

boolTr = head >>= \case
	T.KwTrue  -> return $ BTrue
	T.KwFalse -> return $ BFalse
	_ -> mzero

nameTr = head >>= \case
	T.Name cs -> return cs 
	_ -> mzero
