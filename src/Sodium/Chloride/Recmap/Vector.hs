{-# LANGUAGE RankNTypes #-}
module Sodium.Chloride.Recmap.Vector
	( Recmapper
	, Recmap
	, recmapper
	, recmapper'
	, defaultRecmapper
	, recmap
	, recmapProgram
	, recmapProgram'
	, recmapFunc
	, recmapBody
	, recmapStatement
	, RecmapEnv
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Lens
import Sodium.Chloride.Program.Vector
import qualified Data.Map as M

type RecmapEnv = ReaderT Vars

recmapProgram' :: Recmapper Identity -> Program -> Program
recmapProgram' rm = runIdentity . recmapProgram rm

recmapProgram
	:: (Applicative m, Monad m)
	=> Recmapper m -> Program -> m Program
recmapProgram rm
	= (programFuncs . traversed) (recmapFunc rm)

recmapFunc
	:: (Applicative m, Monad m)
	=> Recmapper m -> Func -> m Func
recmapFunc rm func = runReaderT
	(funcBody (recmap rm) func)
	(func ^. funcSig . funcParams)

data Recmapper m = Recmapper
	{ recmapStatement :: Statement -> RecmapEnv m Statement
	, recmapBody :: Body -> RecmapEnv m Body
	}

defaultRecmapper :: (Applicative m, Monad m) => Recmapper m
defaultRecmapper = Recmapper return return

class Recmap a where
	recmapper
		:: (Applicative m, Monad m)
		=> (a -> RecmapEnv m a) -> Recmapper m
	recmapmod
		:: (Applicative m, Monad m)
		=> Recmapper m -> (a -> RecmapEnv m a)
	recmapdiv
		:: (Applicative m, Monad m)
		=> (forall b. Recmap b => b -> RecmapEnv m b) -> (a -> RecmapEnv m a)

recmap
	:: (Applicative m, Monad m)
	=> Recmapper m -> (forall a. Recmap a => a -> RecmapEnv m a)
recmap rm
	= recmapmod rm <=< recmapdiv (recmap rm)

recmapper'
	:: (Applicative m, Monad m)
	=> Recmap a => (a -> a) -> Recmapper m
recmapper' rm
	= recmapper (return . rm)

instance Recmap Body where
	recmapper rm = defaultRecmapper { recmapBody = rm }
	recmapmod rm = recmapBody rm
	recmapdiv rm body
		= local (M.union $ body ^. bodyVars)
		$ (bodyStatements . traversed . _2) rm body

instance Recmap Statement where
	recmapper rm = defaultRecmapper { recmapStatement = rm }
	recmapmod rm = recmapStatement rm
	recmapdiv rm = onMultiIf <=< onBody <=< onFor where
		onMultiIf = _MultiIfStatement
			$ (multiIfLeafs . traversed . _2) rm <=< multiIfElse rm
		onFor = _ForStatement (forBody rm)
		onBody = _BodyStatement rm
