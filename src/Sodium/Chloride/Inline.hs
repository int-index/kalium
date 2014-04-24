module Sodium.Chloride.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens hiding (Index, Fold)
import qualified Data.Map as M
import Sodium.Chloride.Program.Vector
import Sodium.Chloride.Recmap.Vector
import Sodium.ApplyOnce
import Data.Bool

inline :: Program -> Program
inline = recmapProgram' (recmapper' inlineBody)

inlineBody body
	= update body $ eliminateAssign
		(body ^. bodyResults, body ^. bodyStatements)
	where update body (subResults, subStatements)
		= body
		& bodyResults .~ subResults
		& bodyStatements .~ subStatements

eliminateAssign
	:: ([Expression], [(IndicesList, Statement)])
	-> ([Expression], [(IndicesList, Statement)])
eliminateAssign (bodyResults, (statement:statements))
	= maybe follow id $ do
		([name], Assign expr) <- Just statement
		let subSingle = (,)
			<$> traversed subOnce bodyResults
			<*> (traversed . _2) subOnce statements
		case runReaderT subSingle (name, expr) of
			Once bodyPair -> Just (eliminateAssign bodyPair)
			None bodyPair -> Just (eliminateAssign bodyPair)
			Ambiguous -> Nothing
	where follow
		= over _2 (statement:)
		$ eliminateAssign (bodyResults, statements)
eliminateAssign bodyPair = bodyPair

type SubOnceEnv = ((Name, Index), Expression)

class SubOnce a where
	subOnce :: a -> ReaderT SubOnceEnv ApplyOnce a

apUnless :: Monad m => (a -> m Bool) -> (a -> m a) -> (a -> m a)
apUnless p f = \a -> p a >>= bool (f a) (return a)

instance SubOnce Expression where
	subOnce = \case
		Primary prim -> return (Primary prim)
		Access name' j -> do
			(name, expr) <- ask
			if name == (name', j)
				then lift (Once expr)
				else return (Access name' j)
		Call op exprs
			 -> Call op
			<$> traversed subOnce exprs
		Fold op exprs range
			 -> Fold op
			<$> traversed subOnce exprs
			<*> subOnce range

instance SubOnce Statement where
	subOnce
		 = (_Execute . _2 . traversed) subOnce
		>=> _Assign subOnce
		>=> _BodyStatement    subOnce
		>=> _ForStatement     subOnce
		>=> _MultiIfStatement subOnce

instance SubOnce ForCycle where
	subOnce
		 =  forRange subOnce
		>=> (forArgExprs . traversed) subOnce
		>=> apUnless
			(shadowedBy . toListOf (forArgIndices . traversed . _1))
			(forBody subOnce)

instance SubOnce MultiIfBranch where
	subOnce
		 = (multiIfLeafs . traversed) (_1 subOnce >=> _2 subOnce)
		>=> multiIfElse subOnce

instance SubOnce Body where
	subOnce = apUnless
		(shadowedBy . M.keys . view bodyVars)
		unsafeBodySubOnce

unsafeBodySubOnce
	 =  (bodyStatements . traversed . _2) subOnce
	>=> (bodyResults . traversed) subOnce

shadowedBy :: Monad m => [Name] -> ReaderT SubOnceEnv m Bool
shadowedBy names = do
	(name, _) <- ask
	return $ fst name `elem` names
