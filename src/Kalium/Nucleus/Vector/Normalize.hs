{-# LANGUAGE OverloadedStrings #-}
module Kalium.Nucleus.Vector.Normalize where

import Kalium.Prelude

import Kalium.Nucleus.Vector.Program
import Kalium.Nucleus.Vector.Recmap
import Kalium.Nucleus.Vector.Template
import Kalium.Nucleus.Vector.Cost (estimateSimple)
import Kalium.Util

normalize :: Endo' Program
normalize = over recmapped $ fire
    [ AppOp2 OpFlipMapTaintedIgnore "2" "1" := AppOp2 OpMapTaintedIgnore "1" "2"
    ]

denormalize :: Endo' Program
denormalize = over recmapped $ fire
    [ AppOp2 OpMapTaintedIgnore "1" "2" := "a"
        .:> do
            a1 :: Expression <- mmeta "1"
            a2 :: Expression <- mmeta "2"
            "a" ..= if estimateSimple a1 < estimateSimple a2
                then AppOp2 OpMapTaintedIgnore     a1 a2
                else AppOp2 OpFlipMapTaintedIgnore a2 a1
    ]
