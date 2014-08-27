module Sodium.Chloride.Flatten (flatten) where

import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program.Scalar
import Sodium.Chloride.Recmap.Scalar

flatten :: Program -> Program
flatten = over recmapped flattenBody

flattenBody = bodyStatements %~ concatMap flattenStatement

flattenStatement :: Statement -> [Statement]
flattenStatement (BodyStatement body)
    | M.null (body ^. bodyVars)
    = body ^. bodyStatements
flattenStatement statement = [statement]
