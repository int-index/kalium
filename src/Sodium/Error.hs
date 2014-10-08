module Sodium.Error where

import qualified Sodium.Nucleus.Program as N

import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Sodium.Nucleus.Vectorize as V

data Error = ParseError String Int Int
           | NoAccess   N.Name [N.Name]
           | NoFunction N.Name
           | NoReference
           | UpdateImmutable N.Name
           | InvalidOperation
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
    V.InvalidOperation      -> InvalidOperation
