module Sodium.Haskell.Common where

import Language.Haskell.Exts

pattern HsIdent  moduleName name = Qual (ModuleName moduleName) (Ident  name)
pattern HsSymbol moduleName name = Qual (ModuleName moduleName) (Symbol name)
