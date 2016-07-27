{-# LANGUAGE CApiFFI               #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InterruptibleFFI      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.LibUI.FFI.Wrapped.OSX
    (
      -- ** Webviews
      CUIWebview (..)
    , onLoad
    , uiNewWebview
    , uiWebviewLoadUrl
    , uiWebviewLoadHtml
    , uiWebviewEval

    , HasLoadUrl (..)
    , HasLoadHtml (..)
    , HasEvalJs (..)

      -- * Raw FFI
    , module Graphics.LibUI.FFI.Raw.OSX
    )
  where

import           Control.Concurrent
import           Control.Monad          (when, (>=>))
import           Control.Monad.Loops
import           Foreign                hiding (void)
import           Foreign.C
import           System.IO.Unsafe

import           Graphics.LibUI.FFI.Raw
import           Graphics.LibUI.FFI.Raw.OSX

class HasLoadUrl w where
    loadUrl :: w -> String -> IO ()

class HasLoadHtml w where
    loadHtml :: w -> (String, FilePath) -> IO ()

class HasEvalJs w where
    evalJs :: w -> String -> IO String

-- * Webviews
uiNewWebview = c_uiNewWebview
uiWebviewLoadUrl w s = withCString s (c_uiWebviewLoadUrl w)
uiWebviewLoadHtml w s baseUrl = withCString s $ \s' -> withCString baseUrl $ \baseUrl' ->
    c_uiWebviewLoadHtml w s' baseUrl'
uiWebviewEval w s = withCString s (c_uiWebviewEval w) >>= peekCString

onLoad webview action = do
    f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
    c_uiWebviewOnLoad webview f nullPtr

instance HasLoadUrl CUIWebview where
    loadUrl = uiWebviewLoadUrl

instance HasEvalJs CUIWebview where
    evalJs = uiWebviewEval

instance HasLoadHtml CUIWebview where
    loadHtml w = uncurry (uiWebviewLoadHtml w)
