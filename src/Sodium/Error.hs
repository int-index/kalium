module Sodium.Error where

import qualified Sodium.Nucleus.Program as N

import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Sodium.Nucleus.Vectorize as V
import qualified Sodium.Nucleus.Scalar.Typecheck as T
import qualified Sodium.Pascal.Convert as CP

data Error = ParseError String Int Int
           | NoAccess   N.Name [N.Name]
           | NoFunction N.Name
           | NoReference
           | UpdateImmutable N.Name
           | PasConvError
    deriving (Show)

parseError :: P.ParseError -> Error
parseError e = ParseError name line column
    where pos    = P.errorPos e
          name   = P.sourceName   pos
          line   = P.sourceLine   pos
          column = P.sourceColumn pos

vectorizeError :: V.Error -> Error
vectorizeError = \case
    V.NoAccess name indices -> NoAccess name (M.keys indices)
    V.NoFunction name       -> NoFunction name
    V.UpdateImmutable name  -> UpdateImmutable name
    V.NoReference           -> NoReference

typeError :: T.TypeError -> Error
typeError = \case
    T.NoAccess name vars -> NoAccess name (M.keys vars)
    T.NoFunction name    -> NoFunction name

pasconvError :: CP.TypeError -> Error
pasconvError _ = PasConvError
