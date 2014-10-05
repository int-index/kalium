module Sodium.Nucleus.Pass.Flatten (flatten) where

import Control.Lens
import qualified Data.Map as M

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector

flatten :: Program -> Program
flatten = over recmapped flattenBody

flattenBody :: Body -> Body
flattenBody = bodyBinds %~ concatMap flattenBind

flattenBodyBind :: Body -> ([Bind], Body)
flattenBodyBind body = (freeBinds, set bodyBinds closedBinds body)
    where (freeBinds, closedBinds) = span isFree (body ^. bodyBinds)
          isFree = view (bindPattern . to (\(Pattern xs) -> null xs))

flattenBind :: Bind -> [Bind]
flattenBind bind = maybe [bind] id $ do
    BodyStatement body <- return (bind ^. bindStatement)
    let (binds, body') = flattenBodyBind body
    let cx | nullBody body' = []
           | otherwise = [bindStatement .~ BodyStatement body' $ bind]
    return (binds ++ cx)

nullBody :: Body -> Bool
nullBody body =  (body ^. bodyBinds   . to null)
              && (body ^. bodyResults . to null)
              && (body ^. bodyVars  . to M.null)
