{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InterruptibleFFI           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|
Module: Graphics.LibUI.FFI.Raw.OSX
Description: OSX extras to the libui library
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: GPLv3
Maintainer: tacla.yamada@gmail.com
Stability: experimental
|-}
module Graphics.LibUI.FFI.Raw.OSX where

import           Foreign
import           Foreign.C
import           Foreign.CStorable
import           GHC.Generics
import           Graphics.LibUI.FFI.Raw

-- ** Webviews
-- *** CUIWebview <- uiWebview
-- | A webview
--
-- @
-- -- ...
-- wv <- uiNewWebview
-- wv `loadUrl` "https://google.com"
-- wv `evalJs` "document.write('Buya');"
-- -- ...
-- @
newtype CUIWebview = CUIWebview (Ptr RawWebview)
  deriving(Show, ToCUIControl)
data RawWebview

foreign import capi "ui.h uiWebviewLoadUrl"
    c_uiWebviewLoadUrl :: CUIWebview -> CString -> IO ()

foreign import capi "ui.h uiWebviewLoadHTML"
    c_uiWebviewLoadHtml :: CUIWebview -> CString -> CString -> IO ()

foreign import capi "ui.h uiWebviewOnLoad"
    c_uiWebviewOnLoad :: CUIWebview -> FunPtr (CUIWebview -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiWebviewEval"
    c_uiWebviewEval :: CUIWebview -> CString -> IO CString

foreign import capi "ui.h uiNewWebview"
    c_uiNewWebview :: IO CUIWebview

-- ** Maps
-- *** CUIMapview <- uiMapview
-- | A Map view
--
-- @
-- -- ...
-- mv <- uiNewMapview
-- -- ...
-- @
newtype CUIMapview = CUIMapview (Ptr RawMapview)
  deriving(Show, ToCUIControl)
data RawMapview

foreign import capi "ui.h uiNewMapview"
    c_uiNewMapview :: IO CUIMapview

foreign import capi "ui.h uiMapviewSetRegion"
    c_uiMapviewSetRegion :: CUIMapview -> IO ()

-- ** Menu Items
-- | In OSX, there're APIs for defining keyboard shortcut handlers bound to menu
-- items, without which the UX is really bad. Namely the 'Edit' menu items
-- aren't possible without this (see the `markd` example).
foreign import capi "ui.h uiMenuAppendItemWith"
    c_uiMenuAppendItemWith
       :: CUIMenu
       -> CString
       -- ^ Menu title
       -> CString
       -- ^ Menu key
       -> CString
       -- ^ Menu target selector
       -> IO CUIMenuItem

-- | Like c_uiMenuAppendItemWith, but uses the application "menuManager" as the
-- target
foreign import capi "ui.h uiMenuAppendItemWithDefaultTarget"
    c_uiMenuAppendItemWithDefaultTarget
       :: CUIMenu
       -> CString
       -- ^ Menu title
       -> CString
       -- ^ Menu key
       -> CString
       -- ^ Menu target selector
       -> IO CUIMenuItem
