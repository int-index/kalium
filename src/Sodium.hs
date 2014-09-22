module Sodium (translate) where

import Sodium.Nucleus.Vectorize   (vectorize)
import Sodium.Nucleus.IOMagic     (uncurse)
import Sodium.Nucleus.Side        (side)
import Sodium.Nucleus.Pass.Flatten     (flatten)
import Sodium.Nucleus.Pass.JoinMultiIf (joinMultiIf)
import Sodium.Nucleus.Pass.Inline      (inline)
import Sodium.Nucleus.Pass.FoldMatch   (foldMatch)
import Sodium.Nucleus.Pass.ExtractBody (extractBody)
import Sodium.Nucleus.Pass.Clean       (clean)
import Sodium.Pascal.Parse   (parse)
import Language.Haskell.Exts.Pretty (prettyPrint)
import qualified  Sodium.Pascal.Convert as P (convert)
import qualified Sodium.Haskell.Convert as H (convert)
import Data.Profunctor

translate :: String -> String
translate = dimap fromPascal toHaskell onNucleus where
    fromPascal = P.convert . parse
    toHaskell  = prettyPrint . H.convert
    onNucleus = dimap onScalar onVector vectorize
    onScalar = fff joinMultiIf . side . uncurse
    onVector = fff (flatten . inline . foldMatch . extractBody . clean)

fff f = (!!42) . iterate f
