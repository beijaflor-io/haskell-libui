{-# LANGUAGE CApiFFI               #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InterruptibleFFI      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Provides wrappers to make the imperative C API nicer to use in Haskell
--
-- This module should be enough to match how most imperative languages will
-- work with the foreign library, if you're ok with building your GUI
-- imperatively on the IO Monad, this should be fine
module Graphics.LibUI.FFI.Wrapped
    (
      -- * Basic API
      uiInit
    , uiMain
    , uiQuit
    , uiQueueMain
    , uiOnShouldQuit

      -- * UI Controls
    , CUIControl (..)
    , ToCUIControl (..)
    , ToCUIControlIO (..)
    , uiShow
    , uiHide
    , uiDestroy
    , uiGetParent
    , uiSetParent
    , uiGetTopLevel
    , uiGetVisible
    , uiGetEnabled
    , uiSetEnabled

      -- ** Windows
    , CUIWindow (..)
    , uiNewWindow
    , getBorderless
    , setBorderless
    , getContentSize
    , setContentSize
    , onContentSizeChange
    , getFullscreen
    , setFullscreen

      -- ** Labels
    , CUILabel (..)
    , uiNewLabel

      -- ** Layout Controls
      -- *** Boxes
    , CUIBox (..)
    , uiNewHorizontalBox
    , uiNewVerticalBox

      -- *** Tabs
    , CUITabs (..)
    , uiNewTabs
    , appendTab
    , appendTabMargined
    , removeTab

      -- *** Named Groups
    , CUIGroup (..)
    , uiNewGroup

      -- *** Grids
    , CUIGrid (..)
    , uiNewGrid
    , uiGridAppend
    , uiGridInsertAt
    , UIAlign (..)
    , UIAt (..)

      -- *** Separators
    , CUISeparator (..)
    , uiNewHorizontalSeparator
    , uiNewVerticalSeparator

      -- ** Input Types
      -- *** Buttons
    , CUIButton (..)
    , uiNewButton

      -- *** Checkboxes
    , CUICheckbox (..)
    , uiNewCheckbox

      -- *** Text Inputs
    , CUIEntry (..)
    , uiNewEntry
    , uiNewPasswordEntry
    , uiNewSearchEntry
    , CUISpinbox (..)
    , uiNewSpinbox

      -- *** Sliders
    , CUISlider (..)
    , uiNewSlider

      -- *** Selects
    , CUICombobox (..)
    , uiNewCombobox

    , CUIEditableCombobox (..)
    , uiNewEditableCombobox

      -- *** Radio Buttons
    , CUIRadioButtons (..)
    , uiNewRadioButtons

      -- *** Labeled Forms
    , CUIForm (..)
    , uiNewForm
    , uiFormAppend

      -- *** Date & Time Pickers
    , CUIDateTimePicker (..)
    , uiNewDatePicker
    , uiNewTimePicker
    , uiNewDateTimePicker

      -- *** Font Picker
    , CUIFontButton (..)
    , uiNewFontButton

      -- *** Color Picker
    , CUIColorButton (..)
    , uiNewColorButton

      -- *** Multiline Inputs
    , CUIMultilineEntry (..)
    , appendText
    , uiNewMultilineEntry
    , uiNewNonWrappingMultilineEntry

      -- ** Progress Indicators
    , CUIProgressBar (..)
    , uiNewProgressBar

      -- ** The Menubar
    , CUIMenu (..)
    , uiNewMenu
    , uiMenuAppendItem
    , uiMenuAppendCheckItem
    , uiMenuAppendQuitItem
    , uiMenuAppendPreferencesItem
    , uiMenuAppendAboutItem
    , uiMenuAppendSeparator

    , CUIMenuItem (..)
    , uiMenuItemEnable
    , uiMenuItemDisable

      -- ** UI Alerts and Dialogs
    , uiOpenFile
    , uiSaveFile
    , uiMsgBox
    , uiMsgBoxError

      -- * Type-Classes
    , HasSetTitle (..)
    , HasGetTitle (..)

    , HasSetPosition (..)
    , HasGetPosition (..)

    , HasGetText (..)
    , HasSetText (..)

    , HasSetValue (..)
    , HasGetValue (..)

    , HasGetChecked (..)
    , HasSetChecked (..)

    , HasSetChild (..)
    , HasAppendChild (..)
    , HasRemoveChild (..)

    , HasOnPositionChanged (..)
    , HasOnClicked (..)
    , HasOnChanged (..)
    , HasOnClosing (..)
    , HasOnShouldQuit (..)

    , HasSetPadded (..)
    , HasGetPadded (..)

    , HasSetMargined (..)
    , HasGetMargined (..)

    , HasSetReadOnly (..)
    , HasGetReadOnly (..)

    , HasAppendOption (..)
    , ToAppendInput (..)

    , appendIOChild
    , appendIOChildStretchy

      -- * Internal functions
      -- ** Ticking the loop manually
    , uiMainSteps, uiMainStep, hasMainM, getHasMain, setHasMain

      -- ** Other
    , boolToNum, numToBool, toCUIAlign, toCUIAt, peekCStringSafe

      -- * Raw FFI
    , module Graphics.LibUI.FFI.Raw
    )
  where

import           Control.Concurrent
import           Control.Monad          (when, (>=>))
import           Control.Monad.Loops
import           Foreign                hiding (void)
import           Foreign.C
import           System.IO.Unsafe

import           Graphics.LibUI.FFI.Raw

-- * Basic API

-- | Initialize the UI options. Needs to be called before any UI building
--
-- @
-- main = do
--     uiInit
--     -- ...
--     uiMain
-- @
uiInit :: IO ()
uiInit =
    alloca $ \ptr -> do
        poke ptr (CSize (fromIntegral (sizeOf (CSize 0))))
        c_uiInit ptr

-- | Start the main loop
uiMain :: IO ()
uiMain = do
    uiMainSteps
    -- TODO Replace with uiMainStepExpire or something
    whileM_ getHasMain (uiMainStep 1)

-- | Quit the main loop
uiQuit :: IO ()
uiQuit = do
    setHasMain False
    c_uiQuit

-- |
-- Actions not run on the main thread (that aren't just callbacks), need to be
-- queued with @uiQueueMain@
--
-- It calls 'c_uiQueueMain' under the hood
--
-- @
-- main = do
--     -- .. 'uiInit' & create a window
--     pg <- 'uiNewProgressBar'
--     ^ Create a progressbar
--     'forkIO' $ do
--     'forM_' [0..100] $ \i -> do
--         'threadDelay' (1000 * 100)
--         'uiQueueMain' ('setValue' pg i)
--         ^ Fork a thread
--     -- .. 'setChild' & 'uiMain'
-- @
uiQueueMain :: IO () -> IO ()
uiQueueMain a = do
    m <- getHasMain
    when m $ do
        a' <- c_wrap1 $ \_ -> do
            r <- a
            return ()
        c_uiQueueMain a' nullPtr

-- | Add a hook to before quit
uiOnShouldQuit :: IO Int -> IO ()
uiOnShouldQuit a = do
    f <- castFunPtr <$> c_wrap1I (\_ -> fromIntegral <$> a)
    c_uiOnShouldQuit f nullPtr

-- * Shared API
-- | Controls with `ui...SetTitle` functions
class HasSetTitle s where
    setTitle :: s -> String -> IO ()

-- | Controls with `ui...Title` functions
class HasGetTitle s where
    getTitle :: s -> IO String

-- | Controls with `ui...SetPosition` functions
class HasSetPosition s where
    setPosition :: s -> (Int, Int) -> IO ()

-- | Controls with `ui...Position` functions
class HasGetPosition s where
    getPosition :: s -> IO (Int, Int)

-- | Controls with `ui...Text` functions
class HasGetText s where
    getText :: s -> IO String

-- | Controls with `ui...SetText` functions
class HasSetText s where
    setText :: s -> String -> IO ()

-- | Controls with `ui...SetReadOnly` functions
class HasSetReadOnly s where
    setReadOnly :: s -> Bool -> IO ()

-- | Controls with `ui...ReadOnly` functions
class HasGetReadOnly s where
    getReadOnly :: s -> IO Bool

-- | Controls with `ui...SetValue` functions
class HasSetValue s where
    setValue :: s -> Int -> IO ()

-- | Controls with `ui...GetValue` functions
class HasGetValue s where
    getValue :: s -> IO Int

-- | Controls with `ui...OnClicked` functions
class HasOnClicked s where
    onClick :: s -> IO () -> IO ()

class HasOnPositionChanged s where
    onPositionChanged :: s -> IO () -> IO ()

-- | Controls with `ui...OnChanged` functions
class HasOnChanged s where
    onChange :: s -> IO () -> IO ()

-- | Controls with `ui...SetChecked` functions
class HasSetChecked s where
    setChecked :: s -> Bool -> IO ()

-- | Controls with `ui...Checked` functions
class HasGetChecked s where
    getChecked :: s -> IO Bool

-- | Controls with `ui...SetChild` functions
class HasSetChild s where
    setChild :: ToCUIControlIO a => s -> a -> IO ()

-- | Controls with `ui...Append` functions
class HasAppendChild s where
    -- | Append a child to this control
    appendChild :: ToCUIControlIO a => s -> a -> IO ()
    appendChildStretchy :: ToCUIControlIO a => s -> a -> IO ()
    appendChildStretchy = appendChild

-- | Controls with `ui...Delete` functions
class HasRemoveChild s where
    -- | Remove the child at index from this control
    removeChild :: s -> Int -> IO ()

-- | Append an action returning a child to this control
appendIOChild :: (HasAppendChild s, ToCUIControlIO c) => s -> IO c -> IO ()
appendIOChild container childAction = do
    c <- childAction
    container `appendChild` c

appendIOChildStretchy :: (HasAppendChild s, ToCUIControlIO c) => s -> IO c -> IO ()
appendIOChildStretchy container childAction = do
    c <- childAction
    container `appendChildStretchy` c

class HasOnClosing w where
    onClosing :: w -> IO () -> IO ()

class HasOnShouldQuit w where
    onShouldQuit :: w -> IO () -> IO ()

class HasSetPadded w where
    setPadded :: w -> Bool -> IO ()

class HasGetPadded w where
    getPadded :: w -> IO Bool

class HasSetMargined w where
    setMargined :: w -> Bool -> IO ()

class HasGetMargined w where
    getMargined :: w -> IO Bool

-- * CUIControl API
-- | Displays a control ('c_uiControlShow')
uiShow :: ToCUIControl a => a -> IO ()
uiShow c = c_uiControlShow (toCUIControl c)

-- | Hides a control ('c_uiControlHide')
uiHide :: ToCUIControl a => a -> IO ()
uiHide = c_uiControlHide . toCUIControl

-- | Destroys a control ('c_uiControlDestroy')
uiDestroy :: ToCUIControl a => a -> IO ()
uiDestroy = c_uiControlDestroy . toCUIControl

-- | Get a control's parent ('c_uiControlParent')
uiGetParent :: ToCUIControl a => a -> IO CUIControl
uiGetParent = c_uiControlParent . toCUIControl

-- | Set a control's parent ('c_uiControlSetParent')
uiSetParent :: (ToCUIControl a, ToCUIControl b) => a -> b -> IO ()
uiSetParent control parent =
    c_uiControlSetParent (toCUIControl control) (toCUIControl parent)

-- | Get if a control is on the top level ('c_uiControlTopLevel')
uiGetTopLevel :: ToCUIControl a => a -> IO Bool
uiGetTopLevel c = numToBool <$> c_uiControlToplevel (toCUIControl c)

-- | Get if a control is visible ('c_uiControlVisible')
uiGetVisible :: ToCUIControl a => a -> IO Bool
uiGetVisible c = numToBool <$> c_uiControlVisible (toCUIControl c)

-- | Get if a control is enabled ('c_uiControlEnabled')
uiGetEnabled :: ToCUIControl a => a -> IO Bool
uiGetEnabled c = numToBool <$> c_uiControlEnabled (toCUIControl c)

-- | Set if a control is enabled ('c_uiControlEnable' & 'c_uiControlDisable')
uiSetEnabled :: ToCUIControl a => a -> Bool -> IO ()
uiSetEnabled c True = c_uiControlEnable (toCUIControl c)
uiSetEnabled c False = c_uiControlDisable (toCUIControl c)

-- * UI Controls
-- ** Windows
-- *** CUIWindow <- uiWindow

-- |
-- Wrapped version of `c_uiNewWindow`
uiNewWindow
    :: String
    -- ^ Title
    -> Int
    -- ^ Width
    -> Int
    -- ^ Height
    -> Bool
    -- ^ Has menubar
    -> IO CUIWindow
uiNewWindow t w h hasMenubar =
    withCString t $ \t' -> c_uiNewWindow t' (fromIntegral w) (fromIntegral h) (boolToNum hasMenubar)

setBorderless :: CUIWindow -> Bool -> IO ()
setBorderless w b = c_uiWindowSetBorderless w (boolToNum b)

getBorderless :: CUIWindow -> IO Bool
getBorderless w = numToBool <$> c_uiWindowBorderless w

getContentSize :: CUIWindow -> IO (Int, Int)
getContentSize w = alloca $ \x -> alloca $ \y -> do
    c_uiWindowContentSize w x y
    x' <- peek x
    y' <- peek y
    return (fromIntegral x', fromIntegral y')

setContentSize :: CUIWindow -> (Int, Int) -> IO ()
setContentSize w (x, y) =
    c_uiWindowSetContentSize w (fromIntegral x) (fromIntegral y)

onContentSizeChange w action = do
    f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
    c_uiWindowOnContentSizeChanged w f nullPtr

getFullscreen w = numToBool <$> c_uiWindowFullscreen w
setFullscreen w b = c_uiWindowSetFullscreen w (boolToNum b)

uiWindowGetTitle :: CUIWindow -> IO String
uiWindowGetTitle = c_uiWindowTitle >=> peekCString

instance HasSetTitle CUIWindow where
    setTitle w t = withCString t (c_uiWindowSetTitle w)

instance HasGetTitle CUIWindow where
    getTitle w = c_uiWindowTitle w >>= peekCString

instance HasOnClosing CUIWindow where
    onClosing w a = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> a)
        c_uiWindowOnClosing w f nullPtr

instance HasSetChild CUIWindow where
    setChild w c = do
        c' <- toCUIControlIO c
        c_uiWindowSetChild w c'

instance HasGetMargined CUIWindow where
    getMargined w = do
        m <- c_uiWindowMargined w
        return $ numToBool m

instance HasSetMargined CUIWindow where
    setMargined w m = c_uiWindowSetMargined w (boolToNum m)

-- ** Labels
-- *** CUILabel <- uiLabel
instance HasGetText CUILabel where
    getText c = c_uiLabelText c >>= peekCString

instance HasSetText CUILabel where
    setText c s = withCString s (c_uiLabelSetText c)

uiNewLabel s = withCString s c_uiNewLabel

-- ** Layout
-- *** CUIBox <- uiBox
instance HasAppendChild CUIBox where
    appendChild b c = do
        c' <- toCUIControlIO c
        c_uiBoxAppend b c' 0
    appendChildStretchy b c = do
        c' <- toCUIControlIO c
        c_uiBoxAppend b c' 1

instance HasRemoveChild CUIBox where
    removeChild b i = c_uiBoxDelete b (fromIntegral i)

instance HasGetPadded CUIBox where
    getPadded b = do
        p <- c_uiBoxPadded b
        return (numToBool p)

instance HasSetPadded CUIBox where
    setPadded b p = c_uiBoxSetPadded b (boolToNum p)

uiNewHorizontalBox = c_uiNewHorizontalBox

uiNewVerticalBox = c_uiNewVerticalBox

-- *** CUITabs <- uiTab
appendTab :: ToCUIControlIO c => CUITabs -> (String, c) -> IO ()
appendTab tabs (name, child) = withCString name $ \cname -> do
    c <- toCUIControlIO child
    c_uiTabAppend tabs cname c

removeTab = c_uiTabDelete

appendTabMargined :: ToCUIControlIO c => CUITabs -> (String, c) -> IO ()
appendTabMargined tabs (name, child) = withCString name $ \cname -> do
    c <- toCUIControlIO child
    c_uiTabAppend tabs cname c
    n <- c_uiTabNumPages tabs
    c_uiTabSetMargined tabs (n - 1) 1

instance HasGetMargined (CUITabs, Int) where
    getMargined (tabs, nt) = do
        c <- c_uiTabMargined tabs (fromIntegral nt)
        return $ numToBool c

instance HasSetMargined (CUITabs, Int) where
    setMargined (tabs, nt) i =
        c_uiTabSetMargined tabs (fromIntegral nt) (boolToNum i)

uiNewTabs :: IO CUITabs
uiNewTabs = c_uiNewTab

-- *** CUIGroup <- uiGroup
instance HasSetChild CUIGroup where
    setChild g c = do
        c' <- toCUIControlIO c
        c_uiGroupSetChild g c'

instance HasSetTitle CUIGroup where
    setTitle c t = withCString t (c_uiGroupSetTitle c)

instance HasGetTitle CUIGroup where
    getTitle c = c_uiGroupTitle c >>= peekCString

instance HasGetMargined CUIGroup where
    getMargined g = do
        c <- c_uiGroupMargined g
        return $ numToBool c

instance HasSetMargined CUIGroup where
    setMargined w m = c_uiGroupSetMargined w (boolToNum m)

uiNewGroup s = withCString s c_uiNewGroup

-- *** CUIGrid <- uiGrid
data UIAlign = UIAlignFill
             | UIAlignStart
             | UIAlignCenter
             | UIAlignEnd

toCUIAlign UIAlignFill = CUIAlign 0
toCUIAlign UIAlignStart = CUIAlign 1
toCUIAlign UIAlignCenter = CUIAlign 2
toCUIAlign UIAlignEnd = CUIAlign 3

data UIAt = UIAtLeading
          | UIAtTop
          | UIAtTrailing
          | UIAtBottom

toCUIAt UIAtLeading = CUIAt 0
toCUIAt UIAtTop = CUIAt 1
toCUIAt UIAtTrailing = CUIAt 2
toCUIAt UIAtBottom = CUIAt 3

uiGridAppend
    :: ToCUIControlIO c
    => CUIGrid
    -> c
    -> Int -> Int
    -> Int -> Int
    -> Int -> UIAlign
    -> Int -> UIAlign
    -> IO ()
uiGridAppend grid control left top xspan yspan hexpand halign vexpand valign = do
    control' <- toCUIControlIO control
    c_uiGridAppend
        grid
        control'
        (fromIntegral left)
        (fromIntegral top)
        (fromIntegral xspan)
        (fromIntegral yspan)
        (fromIntegral hexpand)
        (toCUIAlign halign)
        (fromIntegral vexpand)
        (toCUIAlign valign)

uiGridInsertAt
    :: (ToCUIControlIO oldControl, ToCUIControlIO newControl)
    => CUIGrid
    -> oldControl
    -> newControl
    -> UIAt
    -> Int -> Int
    -> Int -> UIAlign
    -> Int -> UIAlign
    -> IO ()
uiGridInsertAt grid ocontrol ncontrol at xspan yspan hexpand halign vexpand valign = do
    ocontrol' <- toCUIControlIO ocontrol
    ncontrol' <- toCUIControlIO ncontrol
    c_uiGridInsertAt
        grid
        ocontrol'
        ncontrol'
        (toCUIAt at)
        (fromIntegral xspan)
        (fromIntegral yspan)
        (fromIntegral hexpand)
        (toCUIAlign halign)
        (fromIntegral vexpand)
        (toCUIAlign valign)

instance HasGetPadded CUIGrid where
    getPadded g = do
        p <- c_uiGridPadded g
        return (numToBool p)

instance HasSetPadded CUIGrid where
    setPadded g p = c_uiGridSetPadded g (boolToNum p)

uiNewGrid = c_uiNewGrid

-- *** CUISeparator <- uiSeparator
uiNewHorizontalSeparator = c_uiNewHorizontalSeparator
uiNewVerticalSeparator = c_uiNewVerticalSeparator

-- ** Input Types
-- *** Buttons
-- **** CUIButton <- uiButton

instance HasOnClicked CUIButton where
    onClick btn action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiButtonOnClicked btn f nullPtr

instance HasGetText CUIButton where
    getText btn = c_uiButtonText btn >>= peekCString

instance HasSetText CUIButton where
    setText btn s = withCString s (c_uiButtonSetText btn)

uiNewButton str = withCString str c_uiNewButton

-- *** CUICheckbox <- uiCheckbox
instance HasSetText CUICheckbox where
    setText btn s = withCString s (c_uiCheckboxSetText btn)

instance HasGetText CUICheckbox where
    getText btn = c_uiCheckboxText btn >>= peekCString

instance HasSetChecked CUICheckbox where
    setChecked c False = c_uiCheckboxSetChecked c 0
    setChecked c True = c_uiCheckboxSetChecked c 1

instance HasGetChecked CUICheckbox where
    getChecked c = numToBool <$> c_uiCheckboxChecked c

onToggled m action = do
    f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
    c_uiCheckboxOnToggled m f nullPtr

instance HasOnChanged CUICheckbox where
    onChange = onToggled

instance HasOnClicked CUICheckbox where
    onClick = onToggled

uiNewCheckbox s = withCString s c_uiNewCheckbox

-- *** CUIEntry <- uiEntry
instance HasSetText CUIEntry where
    setText c s = withCString s (c_uiEntrySetText c)

instance HasGetText CUIEntry where
    getText c = c_uiEntryText c >>= peekCString

instance HasGetReadOnly CUIEntry where
    getReadOnly c = numToBool <$> c_uiEntryReadOnly c

instance HasSetReadOnly CUIEntry where
    setReadOnly c b = c_uiEntrySetReadOnly c (boolToNum b)

instance HasOnChanged CUIEntry where
    onChange btn action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiEntryOnChanged btn f nullPtr

uiNewEntry = c_uiNewEntry
uiNewPasswordEntry = c_uiNewPasswordEntry
uiNewSearchEntry = c_uiNewSearchEntry

-- *** CUISlider <- uiSlider
instance HasGetValue CUISlider where
    getValue c = fromIntegral <$> c_uiSliderValue c

instance HasSetValue CUISlider where
    setValue c i = c_uiSliderSetValue c (fromIntegral i)

instance HasOnChanged CUISlider where
    onChange btn action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiSliderOnChanged btn f nullPtr

uiNewSlider low high = c_uiNewSlider (fromIntegral low) (fromIntegral high)

-- *** CUICombobox <- uiCombobox
class HasAppendOption a where
    appendOption :: a -> String -> IO ()
    appendOptions :: a -> [String] -> IO ()
    appendOptions x = mapM_ (appendOption x)

instance HasGetValue CUICombobox where
    getValue c = fromIntegral <$> c_uiComboboxSelected c

instance HasSetValue CUICombobox where
    setValue c s = c_uiComboboxSetSelected c (fromIntegral s)

instance HasOnChanged CUICombobox where
    onChange c action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiComboboxOnSelected c f nullPtr

instance HasAppendOption CUICombobox where
    appendOption c s = withCString s (c_uiComboboxAppend c)

uiNewCombobox = c_uiNewCombobox

-- *** CUIEditableCombobox <- uiEditableCombobox
instance HasAppendOption CUIEditableCombobox where
    appendOption c s = withCString s (c_uiEditableComboboxAppend c)

instance HasGetText CUIEditableCombobox where
    getText c = c_uiEditableComboboxText c >>= peekCString

instance HasSetText CUIEditableCombobox where
    setText c s = withCString s (c_uiEditableComboboxSetText c)

instance HasOnChanged CUIEditableCombobox where
    onChange btn action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiEditableComboboxOnChanged btn f nullPtr

uiNewEditableCombobox = c_uiNewEditableCombobox

-- *** CUIRadioButtons <- uiRadioButtons
instance HasAppendOption CUIRadioButtons where
    appendOption c s = withCString s (c_uiRadioButtonsAppend c)

instance HasGetValue CUIRadioButtons where
    getValue c = fromIntegral <$> c_uiRadioButtonsSelected c

instance HasSetValue CUIRadioButtons where
    setValue c s = c_uiRadioButtonsSetSelected c (fromIntegral s)

instance HasOnChanged CUIRadioButtons where
    onChange c action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiRadioButtonsOnSelected c f nullPtr

-- TODO setSelected type-class

uiNewRadioButtons = c_uiNewRadioButtons

-- *** CUIForm <- uiForm
uiFormAppend form name input stretchy = withCString name $ \cname ->
    c_uiFormAppend form cname input (boolToNum stretchy)

class ToAppendInput e where
    appendInput :: CUIForm -> e -> IO ()

instance ToCUIControlIO c => ToAppendInput (String, c, Bool) where
    form `appendInput` (name, input, stretchy) = do
        input' <- toCUIControlIO input
        uiFormAppend form name input' stretchy

instance ToCUIControlIO c => ToAppendInput (String, c) where
    form `appendInput` (name, input) = form `appendInput` (name, input, True)

instance HasRemoveChild CUIForm where
    removeChild b i = c_uiFormDelete b (fromIntegral i)

instance HasGetPadded CUIForm where
    getPadded b = do
        p <- c_uiFormPadded b
        return $ numToBool p

instance HasSetPadded CUIForm where
    setPadded b p = c_uiFormSetPadded b (boolToNum p)

uiNewForm = c_uiNewForm

-- *** CUIDatePicker <- uiDatePicker

uiNewDatePicker = c_uiNewDatePicker
uiNewTimePicker = c_uiNewTimePicker
uiNewDateTimePicker = c_uiNewDateTimePicker

-- *** CUIFontButton <- uiFontButton
uiNewFontButton = c_uiNewFontButton

-- *** CUIColorButton <- uiColorButton
uiNewColorButton = c_uiNewColorButton

-- *** CUIMultilineEntry <- uiMultilineEntry
instance HasGetText CUIMultilineEntry where
    getText c = c_uiMultilineEntryText c >>= peekCString

instance HasSetText CUIMultilineEntry where
    setText c s = withCString s (c_uiMultilineEntrySetText c)

instance HasOnChanged CUIMultilineEntry where
    onChange m action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiMultilineEntryOnChanged m f nullPtr

instance HasGetReadOnly CUIMultilineEntry where
    getReadOnly e = numToBool <$> c_uiMultilineEntryReadOnly e

instance HasSetReadOnly CUIMultilineEntry where
    setReadOnly e b = c_uiMultilineEntrySetReadOnly e (boolToNum b)

appendText :: CUIMultilineEntry -> String -> IO ()
appendText m s = withCString s (c_uiMultilineEntryAppend m)

uiNewMultilineEntry = c_uiNewMultilineEntry

uiNewNonWrappingMultilineEntry = c_uiNewNonWrappingMultilineEntry

-- ** Progress Indicators
-- *** CUIProgressBar <- uiProgressBar
instance HasGetValue CUIProgressBar where
    getValue c = fromIntegral <$> c_uiProgressBarValue c

instance HasSetValue CUIProgressBar where
    setValue c i = c_uiProgressBarSetValue c (fromIntegral i)

uiNewProgressBar = c_uiNewProgressBar

-- *** CUISpinbox <- uiSpinbox
instance HasGetValue CUISpinbox where
    getValue c = fromIntegral <$> c_uiSpinboxValue c

instance HasSetValue CUISpinbox where
    setValue c i = c_uiSpinboxSetValue c (fromIntegral i)

instance HasOnChanged CUISpinbox where
    onChange m action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiSpinboxOnChanged m f nullPtr

uiNewSpinbox low high = c_uiNewSpinbox (fromIntegral low) (fromIntegral high)

-- * The Menubar
-- ** CUIMenu <- uiMenu

uiNewMenu s = newCString s >>= c_uiNewMenu
uiMenuAppendItem m s = withCString s (c_uiMenuAppendItem m)
uiMenuAppendCheckItem m s = withCString s (c_uiMenuAppendCheckItem m)
uiMenuAppendQuitItem = c_uiMenuAppendQuitItem
uiMenuAppendAboutItem = c_uiMenuAppendAboutItem
uiMenuAppendPreferencesItem = c_uiMenuAppendPreferencesItem
uiMenuAppendSeparator = c_uiMenuAppendSeparator

-- ** CUIMenuItem <- uiMenuItem
uiMenuItemEnable = c_uiMenuItemEnable
uiMenuItemDisable = c_uiMenuItemDisable

instance HasOnClicked CUIMenuItem where
    onClick itm action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiMenuItemOnClicked itm f nullPtr

instance HasGetChecked CUIMenuItem where
    getChecked c = numToBool <$> c_uiMenuItemChecked c

instance HasSetChecked CUIMenuItem where
    setChecked c False = c_uiMenuItemSetChecked c 0
    setChecked c True = c_uiMenuItemSetChecked c 1

-- * UI Alerts and Dialogs

uiOpenFile :: CUIWindow -> IO (Maybe FilePath)
uiOpenFile wn = do
    cstr <- c_uiOpenFile wn
    peekCStringSafe cstr

uiSaveFile :: CUIWindow -> IO (Maybe FilePath)
uiSaveFile wn = do
    cstr <- c_uiSaveFile wn
    peekCStringSafe cstr

uiMsgBox
    :: CUIWindow
    -> String
    -> String
    -> IO ()
uiMsgBox w t d = withCString t $ \t' -> withCString d $ \d' ->
    c_uiMsgBox w t' d'

uiMsgBoxError
    :: CUIWindow
    -> String
    -> String
    -> IO ()
uiMsgBoxError w t d = withCString t $ \t' -> withCString d $ \d' ->
    c_uiMsgBoxError w t' d'

-- * Internal functions
-- ** Ticking the loop
-- | Setup the main loop to be ticked manually
uiMainSteps :: IO ()
uiMainSteps = setHasMain True >> c_uiMainSteps

-- | Tick the main loop
uiMainStep :: Int -> IO Int
uiMainStep i = do
    ne <- c_uiMainStep (fromIntegral i)
    return (fromIntegral ne)

-- | Is the main loop running?
hasMainM :: MVar Bool
hasMainM = unsafePerformIO (newMVar False)
{-# NOINLINE hasMainM #-}

getHasMain :: IO Bool
getHasMain = readMVar hasMainM

setHasMain :: Bool -> IO ()
setHasMain m = modifyMVar_ hasMainM (const (return m))

boolToNum :: Num a => Bool -> a
boolToNum False = 0
boolToNum True = 1

numToBool :: (Num a, Eq a) => a -> Bool
numToBool 0 = False
numToBool _ = True

peekCStringSafe :: CString -> IO (Maybe String)
peekCStringSafe cstr | cstr == nullPtr = return Nothing
peekCStringSafe cstr = do
    str <- peekCString cstr
    return $ case str of
        "" -> Nothing
        _ -> Just str

