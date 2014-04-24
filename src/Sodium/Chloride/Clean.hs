{-# LANGUAGE ExistentialQuantification #-}
module Sodium.Chloride.Clean (clean) where

import Control.Applicative
import Control.Lens hiding (Index, Fold)
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Bool
import Sodium.Chloride.Program.Vector
import Sodium.Chloride.Recmap.Vector

clean :: Program -> Program
clean = recmapProgram' (recmapper' cleanBody)

cleanBody :: Body -> Body
cleanBody body = (bodyVars %~ M.filterWithKey cc) body where
	cc name _ = runReader (unsafeBodyCheckRef body) name

unsafeBodyCheckRef body = checkRef
	[ CheckRef' (body ^. bodyResults)
	, CheckRef' (body ^.. bodyStatements . traversed . _2)
	]

data CheckRef' = forall a . CheckRef a => CheckRef' a

pairCheckRef' (a, b) = CheckRef' [CheckRef' a, CheckRef' b]

class CheckRef a where
	checkRef :: a -> Reader Name Bool

instance CheckRef CheckRef' where
	checkRef (CheckRef' a) = checkRef a

instance CheckRef a => CheckRef [a] where
	checkRef as = or <$> traversed checkRef as

instance CheckRef Expression where
	checkRef = \case
		Primary _ -> return False
		Access name' _ -> do
			name <- ask
			return (name == name')
		Call _ exprs -> -- Check the operator?
			checkRef exprs
		Fold _ exprs range -> checkRef
			[CheckRef' exprs, CheckRef' range]

instance CheckRef Statement where
	checkRef = \case
		Execute _ exprs -> checkRef exprs
		Assign expr -> checkRef expr
		BodyStatement body -> checkRef body
		ForStatement forCycle -> checkRef forCycle
		MultiIfStatement multiIfBranch -> checkRef multiIfBranch

instance CheckRef ForCycle where
	checkRef forCycle = do
		shadowed <- shadowedBy (forCycle ^.. forArgIndices . traversed . _1)
		let base =
			[ CheckRef' (forCycle ^. forRange)
			, CheckRef' (forCycle ^. forArgExprs)
			]
		let unsh = bool [CheckRef' (forCycle ^. forBody) ] [] shadowed
		checkRef (base ++ unsh)

instance CheckRef MultiIfBranch where
	checkRef multiIfBranch = checkRef
		[ CheckRef' . map pairCheckRef'
			$ (multiIfBranch ^. multiIfLeafs)
		, CheckRef' (multiIfBranch ^. multiIfElse)
		]

instance CheckRef Body where
	checkRef body = do
		shadowed <- shadowedBy (body ^. bodyVars . to M.keys)
		bool (unsafeBodyCheckRef body) (return False) shadowed

shadowedBy :: [Name] -> Reader Name Bool
shadowedBy names = (`elem` names) <$> ask
