{-# LANGUAGE DeriveDataTypeable #-}
module Sodium.Chloride.Vectorize (vectorize) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Chloride.Program.Scalar
import qualified Sodium.Chloride.Program.Vector as Vec

import Control.Exception
import Data.Typeable

import Debug.Trace

data VectorizerException
	= NoAccess Name
	| UpdateImmutable Name
	deriving (Show, Typeable)

instance Exception VectorizerException

vectorize :: Program -> Vec.Program
vectorize
	= Vec.Program
	. map vectorizeFunc
	. view programFuncs

vectorizeFunc :: Func -> Vec.Func
vectorizeFunc func
	= Vec.Func (func ^. funcSig)
	$ vecBodyGen (func ^. funcResults)
	where (_, vecBodyGen)
		= runReader (vectorizeBody (func ^. funcBody))
		$ initIndices (Vec.Index 0) (func ^. funcSig . funcParams)

vectorizeBody :: Body -> Reader Vec.Indices ([Name], [Expression] -> Vec.Body)
vectorizeBody body = reader (vectorizeBodyR body)

vectorizeBodyR body closure = (changed, vecBodyGen) where
	isLocal = flip elem $ M.keys (body ^.bodyVars)
	indices = initIndices Vec.Uninitialized (body ^. bodyVars) `M.union` closure
	vectorizeStatement' statement
		= _1 (mapM registerIndexUpdate)
		=<< readerToState (vectorizeStatement statement)
	(vecStatements, indices')
		= flip runState indices
		$ mapM vectorizeStatement' (body ^. bodyStatements)
	changed
		= M.keys
		$ M.filterWithKey ((&&) . not . isLocal)
		$ M.intersectionWith (/=)
		indices indices'
	vecBodyGen results
		= Vec.Body (body ^. bodyVars) (map (uncurry Vec.Bind) vecStatements)
		$ runReader (mapM vectorizeExpression results) indices'

vectorizeBody' :: Body -> Reader Vec.Indices ([Name], Vec.Body)
vectorizeBody' body = do
	(changed, vecBodyGen) <- vectorizeBody body
	let vecBody = vecBodyGen (map Access changed)
	return (changed, vecBody)

vectorizeStatement :: Statement -> Reader Vec.Indices ([Name], Vec.Statement)
vectorizeStatement = \case
	BodyStatement body
		 -> over _2 Vec.BodyStatement
		<$> vectorizeBody' body
	Assign name expr
		 -> trace "WARNING: Assign statements are to be removed!"
		 $ (,) [name]
		<$> (Vec.Assign <$> vectorizeExpression expr)
	SideCall res op args -> do
		vecArgs <- mapM vectorizeExpression args
		-- TODO: typecheck in order to find out
		-- what lvalues can actually get changed
		let sidenames = []
		let resnames = [res]
		return $ (resnames ++ sidenames, Vec.Assign (Vec.call op vecArgs))
	Execute mres name args -> do
		vecArgs <- mapM vectorizeExpression args
		-- TODO: typecheck in order to find out
		-- what lvalues can actually get changed
		let sidenames = []
		let resnames = maybe empty pure mres
		return $ (resnames ++ sidenames, Vec.Execute name vecArgs)
	ForStatement forCycle -> over _2 Vec.ForStatement <$> do
		vecRange <- vectorizeExpression (forCycle ^. forRange)
		(changed, vecBody)
			<- local (M.insert (forCycle ^. forName) Vec.Immutable)
			 $ vectorizeBody' (forCycle ^. forBody)
		argIndices <- closedIndices changed
		let vecForCycle = Vec.ForCycle
			argIndices
			(uncurry Vec.Access `map` argIndices)
			(forCycle ^. forName)
			vecRange
			vecBody
		return (changed, vecForCycle)
	MultiIfStatement multiIfBranch -> over _2 Vec.MultiIfStatement <$> do
		(changedList, vecLeafGens) <- (unzip <$>)
			$ forM (multiIfBranch ^. multiIfLeafs)
			$ \(expr, body)
				 -> over _2 . (,)
				<$> vectorizeExpression expr
				<*> vectorizeBody body
		(changedElse, vecBodyElseGen)
			<- vectorizeBody (multiIfBranch ^. multiIfElse)
		let changed = nub $ changedElse ++ concat changedList
		let accessChanged = map Access changed
		let vecMultiIfBranch = Vec.MultiIfBranch
			(over _2 ($ accessChanged) `map` vecLeafGens)
			(vecBodyElseGen accessChanged)
		return $ (changed, vecMultiIfBranch)

vectorizeExpression :: Expression -> Reader Vec.Indices Vec.Expression
vectorizeExpression = \case
	Primary a -> return (Vec.Primary a)
	Access name -> Vec.Access name <$> lookupIndex name
	Call name exprs
		 -> trace ("WARNING: Call expressions are to be removed!\n\t" ++ show (Call name exprs))
		 $  Vec.Call name
		<$> mapM vectorizeExpression exprs

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex name
	 =  maybe (throw $ NoAccess name) id
	<$> reader (M.lookup name)

registerIndexUpdate name = do
	index <- readerToState $ lookupIndex name
	let index' = indexUpdate index
	modify $ M.insert name index'
	return (name, index')
	where indexUpdate = \case
		Vec.Index n -> Vec.Index (succ n)
		Vec.Uninitialized -> Vec.Index 0
		Vec.Immutable -> throw $ UpdateImmutable name

closedIndices :: [Name] -> Reader Vec.Indices Vec.IndicesList
closedIndices = mapM $ \name -> (name,) <$> lookupIndex name

initIndices :: Vec.Index -> Vars -> Vec.Indices
initIndices n = M.map (const n)
