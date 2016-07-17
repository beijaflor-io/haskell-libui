{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Info.Class.TH where

import           Language.Haskell.TH
import           System.Info

class TargetOS a where

data MacOS

data Linux

data Windows

makeOS :: Q [Dec]
makeOS = case os of
    "darwin" -> [d| instance TargetOS MacOS where |]
    "linux" -> [d| instance TargetOS MacOS where |]
    "windows" -> [d| instance TargetOS Linux where |]
    -- _ -> []
