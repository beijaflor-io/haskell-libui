{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module System.Info.Class
  where

import           Language.Haskell.TH
import           System.Info.Class.TH

$(makeOS)

fn :: TargetOS MacOS => IO ()
fn = undefined
