module Sodium.Nucleus.Vector.Inline where

import Control.Lens

import Sodium.Nucleus.Vector.Program
import Sodium.Nucleus.Vector.Recmap

inline :: Program -> Program
inline = over recmapped inlineExpression

inlineExpression :: Expression -> Expression
inlineExpression = id
