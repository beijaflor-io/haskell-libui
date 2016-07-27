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

      -- ** Extra menubar operations
    , uiMenuAppendItemWith
    , uiMenuAppendItemWithDefaultTarget

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

-- * Extra menu operations
-- | In OSX, there're APIs for defining keyboard shortcut handlers bound to menu
-- items, without which the UX is really bad. Namely the 'Edit' menu items
-- aren't possible without this (see the `markd` example).
--
-- This is essentially just calling a Cocoa API
uiMenuAppendItemWith m s k sl =
    withCString s $ \s' -> withCString k $ \k' -> withCString sl $ \sl' ->
    c_uiMenuAppendItemWith m s' k' sl'

-- | Like c_uiMenuAppendItemWith, but uses the application "menuManager" as the
-- target
--
-- This is essentially just calling a Cocoa API
uiMenuAppendItemWithDefaultTarget m s k sl =
    withCString s $ \s' -> withCString k $ \k' -> withCString sl $ \sl' ->
    c_uiMenuAppendItemWithDefaultTarget m s' k' sl'
