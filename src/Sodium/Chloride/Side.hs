module Sodium.Chloride.Side (side) where

import Control.Monad.Writer
import qualified Data.Map as M
import Sodium.Chloride.Program.Scalar
import Sodium.Chloride.Recmap.Scalar
import Data.Stack

side :: Program -> Program
side = recmapProgram' (recmapper' sideStatement)

sideStatement :: Statement -> Statement
sideStatement (Assign name expr) = BodyStatement (sideAssign name expr)
sideStatement statement = statement

sideAssign :: Name -> Expression -> Body
sideAssign name expr = Body (M.fromList vardecls) statements where
	statements = sidecalls ++ [SideCall name OpId [e]]
	(e, xs) = evalStack (runWriterT (sideExpression expr)) (map NameGen [0..])
	(vardecls, sidecalls) = unzip xs

sideExpression
	:: Expression
	-> WriterT [((Name, ClType), Statement)] (Stack Name) Expression
sideExpression = \case
	Access name -> return (Access name)
	Primary lit -> return (Primary lit)
	Call op args -> do
		eArgs <- mapM sideExpression args
		name <- pop
		let vardecl = (name, ClVoid) -- TODO: the real type
		tell [(vardecl, SideCall name op eArgs)]
		return (Access name)
