{-# LANGUAGE RankNTypes, ConstraintKinds #-}
module Sodium.Chloride.Recmap.Vector
	( Recmapper
	, Recmap
	, recmapper
	, recmapper'
	, recmap
	, recmapProgram
	, recmapProgram'
	, recmapFunc
	, recmapBody
	, recmapStatement
	, localizer
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Lens
import Sodium.Chloride.Program.Vector
import Data.Monoid

type Monad' a = (Applicative a, Monad a)

recmapProgram' :: Recmapper Identity -> Program -> Program
recmapProgram' rm = runIdentity . recmapProgram rm

recmapProgram :: Monad' m => Recmapper m -> Program -> m Program
recmapProgram rm = (programFuncs . traversed) (recmapFunc rm)

recmapFunc :: Monad' m => Recmapper m -> Func -> m Func
recmapFunc rm func
	= localizer rm (func ^. funcSig . funcParams)
	$ funcBody (recmap rm) func

data Recmapper m = Recmapper
	{ recmapStatement :: Statement -> m Statement
	, recmapBody :: Body -> m Body
	, localizer :: forall a. Vars -> m a -> m a
	}

instance Monad m => Monoid (Recmapper m) where
	mappend rm1 rm2 = Recmapper
		{ recmapStatement = recmapStatement rm1 <=< recmapStatement rm2
		, recmapBody = recmapBody rm1 <=< recmapBody rm2
		, localizer = \vars -> localizer rm1 vars . localizer rm2 vars
		}
	mempty = Recmapper return return (const id)

class Recmap a where
	recmapper :: Monad' m => (a -> m a) -> Recmapper m
	recmapmod :: Monad' m => Recmapper m -> (a -> m a)
	recmapdiv :: Monad' m => Recmapper m -> (a -> m a)

recmap :: Monad' m => Recmapper m -> (forall a. Recmap a => a -> m a)
recmap rm = recmapmod rm <=< recmapdiv rm

recmapper' :: Monad' m => Recmap a => (a -> a) -> Recmapper m
recmapper' rm = recmapper (return . rm)

instance Recmap Body where
	recmapper rm = mempty { recmapBody = rm }
	recmapmod rm = recmapBody rm
	recmapdiv rm body
		= localizer rm (body ^. bodyVars)
		$ (bodyBinds . traversed . bindStatement) (recmap rm) body

instance Recmap Statement where
	recmapper rm = mempty { recmapStatement = rm }
	recmapmod rm = recmapStatement rm
	recmapdiv rm = onMultiIf <=< onBody <=< onFor where
		rr = recmap rm
		onMultiIf = _MultiIfStatement
			$ (multiIfLeafs . traversed . _2) rr <=< multiIfElse rr
		onFor = _ForStatement (forBody rr)
		onBody = _BodyStatement rr
