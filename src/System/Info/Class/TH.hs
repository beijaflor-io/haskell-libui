{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module System.Info.Class.TH where

import           Language.Haskell.TH
import           System.Info

-- | An empty class that defines which OS we're targetting
--
-- @
-- fn :: TargetOS MacOS => IO ()
-- fn = osxSpecificFunctionality
-- @
class TargetOS a where

data MacOS
data Linux
data Windows

makeOS :: Q [Dec]
makeOS = case os of
    "darwin" -> [d| instance TargetOS MacOS where |]
    "linux" -> [d| instance TargetOS Linux where |]
    "mingw32" -> [d| instance TargetOS Windows where |]
    _ -> return []
