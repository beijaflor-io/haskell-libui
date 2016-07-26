{-|
Module: Graphics.LibUI.FFI.Raw.OSX
Description: OSX extras to the libui library
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: MIT
Maintainer: tacla.yamada@gmail.com
Stability: experimental
|-}
module Graphics.LibUI.FFI.Raw.OSX where

import           Foreign
import           Foreign.C
import           Foreign.CStorable
import           GHC.Generics

-- ** Custom Controls
-- *** CUIArea <- uiArea
newtype CUIArea = CUIArea (Ptr RawArea)
  deriving(Show, ToCUIControl)
data RawArea

newtype CUIDrawContext = CUIDrawContext (Ptr RawDrawContext)
  deriving(Show, Generic, CStorable, Storable)
data RawDrawContext

data CUIAreaDrawParams =
    CUIAreaDrawParams { cuiAreaDrawDrawContext :: CUIDrawContext
                      , cuiAreaDrawAreaWidth   :: CDouble
                      , cuiAreaDrawAreaHeight  :: CDouble
                      , cuiAreaDrawClipX       :: CDouble
                      , cuiAreaDrawClipY       :: CDouble
                      , cuiAreaDrawClipWidth   :: CDouble
                      , cuiAreaDrawClipHeight  :: CDouble
                      }
  deriving(Show, CStorable, Generic)

instance Storable CUIAreaDrawParams where
    peek = cPeek
    poke = cPoke
    alignment = cAlignment
    sizeOf = cSizeOf

newtype CUIAreaMouseEvent = CUIAreaMouseEvent (Ptr RawAreaMouseEvent)
  deriving(Show)

data RawAreaMouseEvent

data CUIAreaHandler =
    CUIAreaHandler { cuiAreaHandlerDraw :: FunPtr (Ptr CUIAreaHandler -> CUIArea -> Ptr CUIAreaDrawParams -> IO ())
                   , cuiAreaHandlerMouseEvent :: FunPtr (Ptr CUIAreaHandler -> CUIArea -> CUIAreaMouseEvent -> IO ())
                   }
  deriving(Show, CStorable, Generic)

instance Storable CUIAreaHandler where
    peek = cPeek
    poke = cPoke
    alignment = cAlignment
    sizeOf = cSizeOf

foreign import capi "ui.h uiAreaSetSize"
    c_uiAreaSetSize :: CUIArea -> CInt -> CInt -> IO ()

foreign import capi "ui.h uiAreaQueueRedrawAll"
    c_uiAreaQueueRedrawAll :: CUIArea -> IO ()

foreign import capi "ui.h uiAreaScrollTo"
    c_uiAreaScrollTo
        :: CUIArea
        -> CDouble
        -- ^ x
        -> CDouble
        -- ^ y
        -> CDouble
        -- ^ width
        -> CDouble
        -- ^ height
        -> IO ()

foreign import capi "ui.h uiNewArea"
    c_uiNewArea :: Ptr CUIAreaHandler -> IO CUIArea

foreign import capi "ui.h uiNewScrollingArea"
    c_uiNewScrollingArea :: Ptr CUIAreaHandler -> CInt -> CInt -> IO CUIArea

-- Internal to haskell-libui
-- foreign import capi "haskell/extra.h ui"
--     c_uiAreaSetSize :: CUIArea -> CInt -> CInt -> IO ()

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
