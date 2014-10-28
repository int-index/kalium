module Sodium.Nucleus.Pass.Flatten (flatten) where

import Control.Lens

import Sodium.Nucleus.Program.Vector
import Sodium.Nucleus.Recmap.Vector
import Sodium.Nucleus.Pattern

flatten :: Program -> Program
flatten = over recmapped flattenBody

flattenBody :: Body -> Body
flattenBody = bodyBinds %~ concatMap flattenBind

flattenBodyBind :: Body -> ([Bind Statement], Body)
flattenBodyBind body = (freeBinds, set bodyBinds closedBinds body)
    where (freeBinds, closedBinds) = span isFree (body ^. bodyBinds)
          isFree = view (bindPattern . to patBound . to null)

flattenBind :: Bind Statement -> [Bind Statement]
flattenBind bind = maybe [bind] id $ do
    BodyStatement body <- return (bind ^. bindStatement)
    case bind ^. bindPattern of
        PWildCard -> return (body ^. bodyBinds)
        _ -> do
            let (binds, body') = flattenBodyBind body
            let cx = [bindStatement .~ BodyStatement body' $ bind]
            return (binds ++ cx)
