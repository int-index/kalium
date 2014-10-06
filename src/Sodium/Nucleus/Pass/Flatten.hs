module Sodium.Nucleus.Pass.Flatten (flatten) where

import Control.Lens
import qualified Data.Map as M

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pattern

flatten :: Program -> Program
flatten = over recmapped flattenBody

flattenBody :: Body -> Body
flattenBody = bodyBinds %~ concatMap flattenBind

flattenBodyBind :: Body -> ([Bind], Body)
flattenBodyBind body = (freeBinds, set bodyBinds closedBinds body)
    where (freeBinds, closedBinds) = span isFree (body ^. bodyBinds)
          isFree = view (bindPattern . to patBound . to null)

flattenBind :: Bind -> [Bind]
flattenBind bind = maybe [bind] id $ do
    BodyStatement body <- return (bind ^. bindStatement)
    let (binds, body') = flattenBodyBind body
    let cx | nullBody body' = []
           | otherwise = [bindStatement .~ BodyStatement body' $ bind]
    return (binds ++ cx)

-- probably not needed, because `extractBody` should
-- extract it as `Assign (Primary LitUnit)` which
-- should be eliminated later
nullBody :: Body -> Bool
nullBody body =  (body ^. bodyBinds   . to null)
              && (body ^. bodyResult . to (==Primary LitUnit))
              && (body ^. bodyVars  . to M.null)
