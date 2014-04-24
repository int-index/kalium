module Sodium.Chloride.IOMagic (uncurse) where

import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program.Scalar
import Sodium.Chloride.Recmap.Scalar

data Error
	= NoAccess Name
	deriving (Show)

uncurse :: Program -> Program
uncurse program
	= either (error . show) id
	$ recmapProgram (recmapper uncurseStatement) program

uncurseStatement :: Statement -> RecmapEnv (Either Error) Statement
uncurseStatement = _Execute
	$ \(mres, op, args) -> case op of
		OpReadLn _ -> case (mres, args) of
			(Nothing, [Access name])
				 -> lookupType name
				<&> \t -> (Just name, OpReadLn t, [])
			_ -> error "IOMagic supports only single-value read operations"
		OpPrintLn
			 -> mapM uncurseArgument args
			<&> \args -> (mres, OpPrintLn, args)
		_ -> return (mres, op, args)

uncurseArgument = \case
	-- TODO: apply `show` only to non-String
	-- expressions as soon as typecheck is implemented
	Access name
		 -> lookupType name
		<&> \case
			ClString -> Access name
			_ -> Call OpShow [Access name]
	expr -> return expr

lookupType name = do
	vars <- ask
	lift $ maybe (Left $ NoAccess name) Right (M.lookup name vars)
