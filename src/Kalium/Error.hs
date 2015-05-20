module Kalium.Error where

import Kalium.Prelude

import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Kalium.Pascal.Parse as P
import qualified Kalium.Nucleus.Vectorize as V
import qualified Kalium.Nucleus.Scalar.Typecheck as T
import qualified Kalium.Nucleus.Scalar.Program as S
import qualified Kalium.Pascal.Convert as CP

data Error = ParseError String Int Int
           | NoAccess   String [String]
           | NoFunction String
           | NoReference
           | UpdateImmutable String
           | PasConvError
           | TypeError String [S.Type]
           | NotImplemented String
           | ArgumentMismatch String
           | Insane String
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
    errorInsane = Insane

instance T.Error Error where
    errorNoAccess name vars = NoAccess (show name) (map show $ M.keys vars)
    errorNoFunction name    = NoFunction (show name)
    errorTypeMismatch name  = TypeError (show name)

instance CP.Error Error where
    errorTypecheck  = TypeError "" []
    errorNoAccess   = NoAccess
    errorNoFunction = NoFunction
    errorNotImplemented = NotImplemented
    errorArgumentMismatch = ArgumentMismatch
