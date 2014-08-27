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
inline = over recmapped inlineBody

inlineBody body
	= update body $ eliminateAssign
		(body ^. bodyResults, body ^. bodyBinds)
	where update body (subResults, subBinds)
		= body
		& bodyResults .~ subResults
		& bodyBinds .~ subBinds

eliminateAssign
	:: ([Expression], [Bind])
	-> ([Expression], [Bind])
eliminateAssign (bodyResults, (bind:binds))
	= maybe follow id $ do
		Bind [name] (Assign expr) <- Just bind
		let subSingle = (,)
			<$> traversed subOnce bodyResults
			<*> traversed subOnce binds
		case runReaderT subSingle (name, expr) of
			Once bodyPair -> Just (eliminateAssign bodyPair)
			None bodyPair -> Just (eliminateAssign bodyPair)
			Ambiguous -> Nothing
	where follow
		= over _2 (bind:)
		$ eliminateAssign (bodyResults, binds)
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

instance SubOnce Bind where
	subOnce = bindStatement subOnce

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
	 =  (bodyBinds   . traversed) subOnce
	>=> (bodyResults . traversed) subOnce

shadowedBy :: Monad m => [Name] -> ReaderT SubOnceEnv m Bool
shadowedBy names = do
	(name, _) <- ask
	return $ fst name `elem` names
