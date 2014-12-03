module Sodium.Error where

import qualified Sodium.Nucleus.Program as N

import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Sodium.Pascal.Parse as P
import qualified Sodium.Nucleus.Vectorize as V
import qualified Sodium.Nucleus.Scalar.Typecheck as T
import qualified Sodium.Pascal.Convert as CP

data Error = ParseError String Int Int
           | NoAccess   String [String]
           | NoFunction String
           | NoReference
           | UpdateImmutable String
           | PasConvError
           | TypeError String [N.Type]
    deriving (Show)

instance P.Error Error where
    errorParse e = ParseError name line column
        where pos    = P.errorPos e
              name   = P.sourceName   pos
              line   = P.sourceLine   pos
              column = P.sourceColumn pos

instance V.Error Error where
    errorNoAccess name names  = NoAccess (show name) (map show names)
    errorUpdateImmutable name = UpdateImmutable (show name)

instance T.Error Error where
    errorNoAccess name vars = NoAccess (show name) (map show $ M.keys vars)
    errorNoFunction name    = NoFunction (show name)
    errorTypeMismatch name  = TypeError (show name)

instance CP.Error Error where
    errorTypecheck  = PasConvError
    errorNoAccess   = PasConvError
    errorNoFunction = PasConvError
