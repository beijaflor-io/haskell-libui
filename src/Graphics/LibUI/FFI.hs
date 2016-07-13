{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InterruptibleFFI           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Provides a raw Haskell C FFI interface with libui
-- This is fully untyped, passing void pointers everywhere.
module Graphics.LibUI.FFI
  where

import           Control.Monad ((>=>))
import           Foreign       hiding (void)
import           Foreign.C

-- * Basic API

-- |
-- At the moment the FFI doesn't care about any of the types.
type VoidPtr = Ptr ()

boolToCInt False = 0
boolToCInt True = 1

cintToBool 0 = False
cintToBool _ = True

-- |
-- Start the main loop. Will block a thread, use 'runUILoop' instead.
--
-- 'c_uiInit' is required for proper window switcher and menubar support.
foreign import capi safe "ui.h uiMain"
    c_uiMain :: IO ()

-- |
-- Initialize main loop
foreign import capi safe "ui.h uiMainSteps"
    c_uiMainSteps :: IO ()

-- |
-- Step through the UI loop
foreign import capi safe "ui.h uiMainStep"
    c_uiMainStep
      :: CInt
      -- ^ How many events to block for
      -> IO CInt
      -- ^ How many events were handled

-- |
-- Destroy the UI
foreign import capi "ui.h uiQuit"
    c_uiQuit :: IO ()

-- |
-- Uninitialize the UI options
foreign import capi "ui.h uiUninit"
    c_uiUninit :: IO ()

-- |
-- Initialize the UI options
foreign import capi "ui.h uiInit"
    c_uiInit :: Ptr CSize -> IO ()

-- |
-- Initialize the UI
foreign import capi "ui.h uiOnShouldQuit"
    c_uiOnShouldQuit :: FunPtr (VoidPtr -> IO CInt) -> VoidPtr -> IO ()

-- ** CUIControl
-- libui is decently object-oriented though written in C
--
-- All objects are subclasses of uiControl and casted to it for general
-- operations; we get a similar interface with type-safety

-- |
-- 'CUIControl' is a `uiControl`
newtype CUIControl = CUIControl VoidPtr

class ToCUIControl a where
    toCUIControl :: a -> CUIControl

instance ToCUIControl CUIControl where
    toCUIControl = id

instance ToCUIControl (Ptr a) where
    toCUIControl = CUIControl . castPtr

class ToCUIControlIO a where
    toCUIControlIO :: a -> IO CUIControl

instance ToCUIControl a => ToCUIControlIO a where
    toCUIControlIO = return . toCUIControl

class HasSetText s where
    setText :: s -> String -> IO ()

class HasOnClicked s where
    onClick :: s -> IO () -> IO ()

class HasSetChecked s where
    setChecked :: s -> Bool -> IO ()

uiShow :: ToCUIControl a => a -> IO ()
uiShow = c_uiControlShow . toCUIControl

uiHide :: ToCUIControl a => a -> IO ()
uiHide = c_uiControlHide . toCUIControl

uiDestroy :: ToCUIControl a => a -> IO ()
uiDestroy = c_uiControlDestroy . toCUIControl

uiParent :: ToCUIControl a => a -> IO CUIControl
uiParent = c_uiControlParent . toCUIControl

uiSetParent :: (ToCUIControl a, ToCUIControl b) => a -> b -> IO ()
uiSetParent control parent =
    c_uiControlSetParent (toCUIControl control) (toCUIControl parent)

uiControlTopLevel :: ToCUIControl a => a -> IO Bool
uiControlTopLevel c = cintToBool <$> c_uiControlToplevel (toCUIControl c)

uiControlVisible :: ToCUIControl a => a -> IO Bool
uiControlVisible c = cintToBool <$> c_uiControlVisible (toCUIControl c)

uiControlEnabled :: ToCUIControl a => a -> IO Bool
uiControlEnabled c = cintToBool <$> c_uiControlEnabled (toCUIControl c)

uiControlEnable :: ToCUIControl a => a -> IO ()
uiControlEnable c = c_uiControlEnable (toCUIControl c)

uiControlDisable :: ToCUIControl a => a -> IO ()
uiControlDisable c = c_uiControlDisable (toCUIControl c)

foreign import capi "ui.h uiControlDestroy"
    c_uiControlDestroy :: CUIControl -> IO ()

foreign import capi "ui.h uiControlParent"
    c_uiControlParent :: CUIControl -> IO CUIControl

foreign import capi "ui.h uiControlSetParent"
    c_uiControlSetParent :: CUIControl -> CUIControl -> IO ()

foreign import capi "ui.h uiControlToplevel"
    c_uiControlToplevel :: CUIControl -> IO CInt

foreign import capi "ui.h uiControlVisible"
    c_uiControlVisible :: CUIControl -> IO CInt

foreign import capi "ui.h uiControlShow"
    c_uiControlShow :: CUIControl -> IO ()

foreign import capi "ui.h uiControlHide"
    c_uiControlHide :: CUIControl -> IO ()

foreign import capi "ui.h uiControlEnabled"
    c_uiControlEnabled :: CUIControl -> IO CInt

foreign import capi "ui.h uiControlEnable"
    c_uiControlEnable :: CUIControl -> IO ()

foreign import capi "ui.h uiControlDisable"
    c_uiControlDisable :: CUIControl -> IO ()

-- ** Functions for creating callbacks to pass to C and call back to Haskell

-- |
-- Wrap a success callback on a foreign pointer
foreign import ccall "wrapper"
    c_wrap1I :: (VoidPtr -> IO CInt) -> IO (FunPtr (VoidPtr -> IO CInt))

-- |
-- Wrap a 1 argument event listener on a foreign pointer
foreign import ccall "wrapper"
    c_wrap1 :: (VoidPtr -> IO ()) -> IO (FunPtr (VoidPtr -> IO ()))

-- |
-- Wrap a 2 argument event listener on a foreign pointer
foreign import ccall "wrapper"
    c_wrap2 :: (VoidPtr -> VoidPtr -> IO ()) -> IO (FunPtr (VoidPtr -> VoidPtr -> IO ()))

-- * UI Controls

-- ** Windows
-- *** CUIWindow <- uiWindow
newtype CUIWindow = CUIWindow (Ptr RawWindow)
  deriving(Show, ToCUIControl)
data RawWindow

-- | Get the window title
foreign import capi "ui.h uiWindowTitle"
    c_uiWindowTitle :: CUIWindow -> IO CString

-- | Set the window title
foreign import capi "ui.h uiWindowSetTitle"
    c_uiWindowSetTitle :: CUIWindow -> CString -> IO ()

-- | Get the window position
foreign import capi "ui.h uiWindowPosition"
    c_uiWindowPosition :: CUIWindow -> Ptr CInt -> Ptr CInt -> IO ()

-- | Set the window position
foreign import capi "ui.h uiWindowSetPosition"
    c_uiWindowSetPosition :: CUIWindow -> CInt -> CInt -> IO ()

-- | Center the window
foreign import capi "ui.h uiWindowCenter"
    c_uiWindowCenter :: CUIWindow -> IO ()

-- | Add a callback to the window's position
foreign import capi "ui.h uiWindowOnPositionChanged"
    c_uiWindowOnPositionChanged :: CUIWindow -> FunPtr (CUIWindow -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

-- | Get the size of the window's content
foreign import capi "ui.h uiWindowContentSize"
    c_uiWindowContentSize :: CUIWindow -> Ptr CInt -> Ptr CInt -> IO ()

-- | Set the size of the window's content
foreign import capi "ui.h uiWindowSetContentSize"
    c_uiWindowSetContentSize :: CUIWindow -> CInt -> CInt -> IO ()

-- | Is the window full-screen?
foreign import capi "ui.h uiWindowFullscreen"
    c_uiWindowFullscreen :: CUIWindow -> IO CInt

-- | Set the window to be full-screen
foreign import capi "ui.h uiWindowSetFullscreen"
    c_uiWindowSetFullscreen :: CUIWindow -> CInt -> IO ()

-- | Add a callback to when content changes
foreign import capi "ui.h uiWindowOnContentSizeChanged"
    c_uiWindowOnContentSizeChanged :: CUIWindow -> FunPtr (CUIWindow -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

-- | Add a callback to when the window is closed
foreign import capi "ui.h uiWindowOnClosing"
    c_uiWindowOnClosing :: CUIWindow -> FunPtr (CUIWindow -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

-- | Is the window borderless?
foreign import capi "ui.h uiWindowBorderless"
    c_uiWindowBorderless :: CUIWindow -> IO CInt

-- | Set the window as borderless
foreign import capi "ui.h uiWindowSetBorderless"
    c_uiWindowSetBorderless :: CUIWindow -> CInt -> IO ()

-- | Set a child on the window
foreign import capi "ui.h uiWindowSetChild"
    c_uiWindowSetChild :: CUIWindow -> CUIControl -> IO ()

-- | Is the window margined
foreign import capi "ui.h uiWindowMargined"
    c_uiWindowMargined :: CUIWindow -> IO CInt

-- | Make the window margined
foreign import capi "ui.h uiWindowSetMargined"
    c_uiWindowSetMargined :: CUIWindow -> CInt -> IO ()

-- | Create a new window
foreign import capi "ui.h uiNewWindow"
    c_uiNewWindow :: CString -> CInt -> CInt -> CInt -> IO CUIWindow

-- ** Buttons
-- *** CUIButton <- uiButton
newtype CUIButton = CUIButton (Ptr RawButton)
  deriving(Show, ToCUIControl)
data RawButton

foreign import capi "ui.h uiButtonOnClicked"
    c_uiButtonOnClicked :: CUIButton -> FunPtr (CUIButton -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

instance HasOnClicked CUIButton where
    onClick btn action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiButtonOnClicked btn f nullPtr

foreign import capi "ui.h uiButtonSetText"
    c_uiButtonSetText :: CUIButton -> CString -> IO ()

instance HasSetText CUIButton where
    setText btn = newCString >=> c_uiButtonSetText btn

foreign import capi "ui.h uiButtonText"
    c_uiButtonText :: CUIButton -> CString

foreign import capi "ui.h uiNewButton"
    c_uiNewButton :: CString -> IO CUIButton

-- ** Layout
-- *** CUIBox <- uiBox
newtype CUIBox = CUIBox (Ptr RawBox)
  deriving(Show, ToCUIControl)
data RawBox

-- |
-- Appends an item to the box
foreign import capi "ui.h uiBoxAppend"
    c_uiBoxAppend
        :: CUIBox
        -> CUIControl
        -> CInt
        -- ^ Whether the box is stretchy
        -> IO ()

foreign import capi "ui.h uiBoxDelete"
    c_uiBoxDelete :: CUIBox -> CInt -> IO ()

foreign import capi "ui.h uiBoxPadded"
    c_uiBoxPadded :: CUIBox -> IO CInt

foreign import capi "ui.h uiBoxSetPadded"
    c_uiBoxSetPadded :: CUIBox -> CInt -> IO ()

foreign import capi "ui.h uiNewHorizontalBox"
    c_uiNewHorizontalBox :: IO CUIBox

foreign import capi "ui.h uiNewVerticalBox"
    c_uiNewVerticalBox :: IO CUIBox

-- ** Input Types
-- *** CUICheckbox <- uiCheckbox
newtype CUICheckbox = CUICheckbox (Ptr RawCheckbox)
  deriving(Show, ToCUIControl)
data RawCheckbox

foreign import capi "ui.h uiCheckboxText"
    c_uiCheckboxText :: CUICheckbox -> IO CString

foreign import capi "ui.h uiCheckboxSetText"
    c_uiCheckboxSetText :: CUICheckbox -> CString -> IO ()

instance HasSetText CUICheckbox where
    setText btn = newCString >=> c_uiCheckboxSetText btn

foreign import capi "ui.h uiCheckboxOnToggled"
    c_uiCheckboxOnToggled :: CUICheckbox -> FunPtr (CUICheckbox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiCheckboxChecked"
    c_uiCheckboxChecked :: CUICheckbox -> IO CInt

foreign import capi "ui.h uiCheckboxSetChecked"
    c_uiCheckboxSetChecked :: CUICheckbox -> CInt -> IO ()

instance HasSetChecked CUICheckbox where
    setChecked c False = c_uiCheckboxSetChecked c 0
    setChecked c True = c_uiCheckboxSetChecked c 1

foreign import capi "ui.h uiNewCheckbox"
    c_uiNewCheckbox :: CString -> IO CUICheckbox

-- *** CUIEntry <- uiEntry
newtype CUIEntry = CUIEntry (Ptr RawEntry)
  deriving(Show, ToCUIControl)
data RawEntry

foreign import capi "ui.h uiEntryText"
    c_uiEntryText :: CUIEntry -> IO CString

foreign import capi "ui.h uiEntrySetText"
    c_uiEntrySetText :: CUIEntry -> CString -> IO ()

instance HasSetText CUIEntry where
    setText c = newCString >=> c_uiEntrySetText c

foreign import capi "ui.h uiEntryOnChanged"
    c_uiEntryOnChanged :: CUIEntry -> FunPtr (CUIEntry -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiEntryReadOnly"
    c_uiEntryReadOnly :: CUIEntry -> IO CInt

foreign import capi "ui.h uiEntrySetReadOnly"
    c_uiEntrySetReadOnly :: CUIEntry -> CInt -> IO ()

foreign import capi "ui.h uiNewEntry"
    c_uiNewEntry :: IO CUIEntry

foreign import capi "ui.h uiNewPasswordEntry"
    c_uiNewPasswordEntry :: IO CUIEntry

foreign import capi "ui.h uiNewSearchEntry"
    c_uiNewSearchEntry :: IO CUIEntry

-- ** CUILabel <- uiLabel
newtype CUILabel = CUILabel (Ptr RawLabel)
  deriving(Show, ToCUIControl)
data RawLabel

foreign import capi "ui.h uiLabelText"
    c_uiLabelText :: CUILabel -> IO CString

foreign import capi "ui.h uiLabelSetText"
    c_uiLabelSetText :: CUILabel -> CString -> IO ()

instance HasSetText CUILabel where
    setText c = newCString >=> c_uiLabelSetText c

foreign import capi "ui.h uiNewLabel"
    c_uiNewLabel :: CString -> IO CUILabel

-- ** CUITab <- uiTab
newtype CUITab = CUITab (Ptr RawTab)
  deriving(Show, ToCUIControl)
data RawTab

foreign import capi "ui.h uiTabAppend"
    c_uiTabAppend
      :: CUITab
      -- ^ The tab group pointer
      -> CString
      -- ^ The tab title
      -> CUIControl
      -- ^ The tab contents
      -> IO ()

foreign import capi "ui.h uiTabInsertAt"
    c_uiTabInsertAt :: CUITab -> CString -> CInt -> CUIControl -> IO ()

foreign import capi "ui.h uiTabDelete"
    c_uiTabDelete :: CUITab -> CInt -> IO ()

foreign import capi "ui.h uiTabNumPages"
    c_uiTabNumPages :: CUITab -> IO CInt

foreign import capi "ui.h uiTabMargined"
    c_uiTabMargined :: CUITab -> CInt -> IO CInt

foreign import capi "ui.h uiTabSetMargined"
    c_uiTabSetMargined :: CUITab -> CInt -> CInt -> IO ()

foreign import capi "ui.h uiNewTab"
    c_uiNewTab :: IO CUITab

-- ** CUIGroup <- uiGroup
newtype CUIGroup = CUIGroup (Ptr RawGroup)
  deriving(Show, ToCUIControl)
data RawGroup

foreign import capi "ui.h uiGroupTitle"
    c_uiGroupTitle :: CUIGroup -> IO CString

foreign import capi "ui.h uiGroupSetTitle"
    c_uiGroupSetTitle :: CUIGroup -> CString -> IO ()

foreign import capi "ui.h uiGroupSetChild"
    c_uiGroupSetChild :: CUIGroup -> CUIControl -> IO ()

foreign import capi "ui.h uiGroupMargined"
    c_uiGroupMargined :: CUIGroup -> IO CInt

foreign import capi "ui.h uiGroupSetMargined"
    c_uiGroupSetMargined :: CUIGroup -> CInt -> IO ()

foreign import capi "ui.h uiNewGroup"
    c_uiNewGroup :: CString -> IO CUIGroup

-- ** CUISpinbox <- uiSpinbox
newtype CUISpinbox = CUISpinbox (Ptr RawSpinbox)
  deriving(Show, ToCUIControl)
data RawSpinbox

foreign import capi "ui.h uiSpinboxValue"
    c_uiSpinboxValue :: CUISpinbox -> IO CInt

foreign import capi "ui.h uiSpinboxSetValue"
    c_uiSpinboxSetValue :: CUISpinbox -> CInt -> IO ()

foreign import capi "ui.h uiSpinboxOnChanged"
    c_uiSpinboxOnChanged :: CUISpinbox -> FunPtr (CUISpinbox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewSpinbox"
    c_uiNewSpinbox :: CInt -> CInt -> IO CUISpinbox

-- ** CUISlider <- uiSlider
newtype CUISlider = CUISlider (Ptr RawSlider)
  deriving(Show, ToCUIControl)
data RawSlider

foreign import capi "ui.h uiSliderValue"
    c_uiSliderValue :: CUISlider -> IO CInt

foreign import capi "ui.h uiSliderSetValue"
    c_uiSliderSetValue :: CUISlider -> CInt -> IO ()

foreign import capi "ui.h uiSliderOnChanged"
    c_uiSliderOnChanged :: CUISlider -> FunPtr (CUISlider -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewSlider"
    c_uiNewSlider :: CInt -> CInt -> IO CUISlider

-- ** CUIProgressBar <- uiProgressBar
newtype CUIProgressBar = CUIProgressBar (Ptr RawProgressBar)
  deriving(Show, ToCUIControl)
data RawProgressBar

foreign import capi "ui.h uiProgressBarValue"
    c_uiProgressBarValue :: CUIProgressBar -> IO CInt

foreign import capi "ui.h uiProgressBarSetValue"
    c_uiProgressBarSetValue :: CUIProgressBar -> CInt -> IO ()

foreign import capi "ui.h uiNewProgressBar"
    c_uiNewProgressBar :: IO CUIProgressBar

-- ** CUISeparator <- uiSeparator
newtype CUISeparator = CUISeparator (Ptr RawSeparator)
  deriving(Show, ToCUIControl)
data RawSeparator

foreign import capi "ui.h uiNewHorizontalSeparator"
    c_uiNewHorizontalSeparator :: IO CUISeparator

foreign import capi "ui.h uiNewVerticalSeparator"
    c_uiNewVerticalSeparator :: IO CUISeparator

-- ** CUICombobox <- uiCombobox
newtype CUICombobox = CUICombobox (Ptr RawCombobox)
  deriving(Show, ToCUIControl)
data RawCombobox

foreign import capi "ui.h uiComboboxAppend"
    c_uiComboboxAppend :: CUICombobox -> CUIControl -> IO ()

foreign import capi "ui.h uiComboboxSelected"
    c_uiComboboxSelected :: CUICombobox -> IO CInt

foreign import capi "ui.h uiComboboxSetSelected"
    c_uiComboboxSetSelected :: CUICombobox -> CInt -> IO ()

foreign import capi "ui.h uiComboboxOnSelected"
    c_uiComboboxOnSelected :: CUICombobox -> FunPtr (CUICombobox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewCombobox"
    c_uiNewCombobox :: IO CUICombobox

-- ** CUIEditableCombobox <- uiEditableCombobox
newtype CUIEditableCombobox = CUIEditableCombobox (Ptr RawEditableCombobox)
  deriving(Show, ToCUIControl)
data RawEditableCombobox

foreign import capi "ui.h uiEditableComboboxAppend"
    c_uiEditableComboboxAppend :: CUIEditableCombobox -> CUIControl -> IO ()

foreign import capi "ui.h uiEditableComboboxText"
    c_uiEditableComboboxText :: CUIEditableCombobox -> IO CString

foreign import capi "ui.h uiEditableComboboxSetText"
    c_uiEditableComboboxSetText :: CUIEditableCombobox -> CString -> IO ()

instance HasSetText CUIEditableCombobox where
    setText c = newCString >=> c_uiEditableComboboxSetText c

foreign import capi "ui.h uiEditableComboboxOnChanged"
    c_uiEditableComboboxOnChanged :: CUIEditableCombobox -> FunPtr (CUIEditableCombobox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewEditableCombobox"
    c_uiNewEditableCombobox :: IO CUIEditableCombobox

-- ** CUIRadioButtons <- uiRadioButtons
newtype CUIRadioButtons = CUIRadioButtons (Ptr RawRadioButtons)
  deriving(Show, ToCUIControl)
data RawRadioButtons

foreign import capi "ui.h uiRadioButtonsAppend"
    c_uiRadioButtonsAppend :: CUIRadioButtons -> CString -> IO ()

foreign import capi "ui.h uiRadioButtonsSelected"
    c_uiRadioButtonsSelected :: CUIRadioButtons -> IO CInt

foreign import capi "ui.h uiRadioButtonsSetSelected"
    c_uiRadioButtonsSetSelected :: CUIRadioButtons -> CInt -> IO ()

foreign import capi "ui.h uiRadioButtonsOnSelected"
    c_uiRadioButtonsOnSelected :: CUIRadioButtons -> FunPtr (CUIRadioButtons -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewRadioButtons"
    c_uiNewRadioButtons :: IO CUIRadioButtons

-- ** CUIForm <- uiForm
newtype CUIForm = CUIForm (Ptr RawForm)
  deriving(Show, ToCUIControl)
data RawForm

foreign import capi "ui.h uiFormAppend"
    c_uiFormAppend
      :: CUIForm
      -- ^ The form pointer
      -> CString
      -- ^ The input label
      -> CUIControl
      -- ^ The input control
      -> CInt
      -- ^ Whether the child is stretchy
      -> IO ()

foreign import capi "ui.h uiFormDelete"
    c_uiFormDelete
      :: CUIForm
      -- ^ The form pointer
      -> CInt
      -- ^ The index to remove
      -> IO ()

foreign import capi "ui.h uiFormPadded"
    c_uiFormPadded :: CUIForm -> IO CInt

foreign import capi "ui.h uiFormSetPadded"
    c_uiFormSetPadded :: CUIForm -> CInt -> IO ()

foreign import capi "ui.h uiNewForm"
    c_uiNewForm :: IO CUIForm

-- ** CUIMultilineEntry <- uiMultilineEntry
newtype CUIMultilineEntry = CUIMultilineEntry (Ptr RawMultilineEntry)
  deriving(Show, ToCUIControl)
data RawMultilineEntry

foreign import capi "ui.h uiMultilineEntryText"
    c_uiMultilineEntryText :: CUIMultilineEntry -> IO CString

foreign import capi "ui.h uiMultilineEntrySetText"
    c_uiMultilineEntrySetText :: CUIMultilineEntry -> CString -> IO ()

instance HasSetText CUIMultilineEntry where
    setText c = newCString >=> c_uiMultilineEntrySetText c

foreign import capi "ui.h uiMultilineEntryAppend"
    c_uiMultilineEntryAppend :: CUIMultilineEntry -> CString -> IO ()

foreign import capi "ui.h uiMultilineEntryOnChanged"
    c_uiMultilineEntryOnChanged :: CUIMultilineEntry -> FunPtr (CUIMultilineEntry -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiMultilineEntryReadOnly"
    c_uiMultilineEntryReadOnly :: CUIMultilineEntry -> IO CInt

foreign import capi "ui.h uiMultilineEntrySetReadOnly"
    c_uiMultilineEntrySetReadOnly :: CUIMultilineEntry -> CInt -> IO ()

foreign import capi "ui.h uiNewMultilineEntry"
    c_uiNewMultilineEntry :: IO CUIMultilineEntry

foreign import capi "ui.h uiNewNonWrappingMultilineEntry"
    c_uiNewNonWrappingMultilineEntry :: IO CUIMultilineEntry

-- ** CUIMenuItem <- uiMenuItem
newtype CUIMenuItem = CUIMenuItem (Ptr RawMenuItem)
  deriving(Show, ToCUIControl)
data RawMenuItem

foreign import capi "ui.h uiMenuItemEnable"
    c_uiMenuItemEnable :: CUIMenuItem -> IO ()

foreign import capi "ui.h uiMenuItemDisable"
    c_uiMenuItemDisable :: CUIMenuItem -> IO ()

foreign import capi "ui.h uiMenuItemOnClicked"
    c_uiMenuItemOnClicked :: CUIMenuItem -> FunPtr (CUIMenuItem -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

instance HasOnClicked CUIMenuItem where
    onClick itm action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiMenuItemOnClicked itm f nullPtr

foreign import capi "ui.h uiMenuItemChecked"
    c_uiMenuItemChecked :: CUIMenuItem -> IO CInt

foreign import capi "ui.h uiMenuItemSetChecked"
    c_uiMenuItemSetChecked :: CUIMenuItem -> CInt -> IO ()

instance HasSetChecked CUIMenuItem where
    setChecked c False = c_uiMenuItemSetChecked c 0
    setChecked c True = c_uiMenuItemSetChecked c 1

-- ** CUIMenu <- uiMenu
newtype CUIMenu = CUIMenu (Ptr RawMenu)
  deriving(Show, ToCUIControl)
data RawMenu

foreign import capi "ui.h uiMenuAppendItem"
    c_uiMenuAppendItem :: CUIMenu -> CString -> IO CUIMenuItem

foreign import capi "ui.h uiMenuAppendCheckItem"
    c_uiMenuAppendCheckItem :: CUIMenu -> CString -> IO CUIMenuItem

foreign import capi "ui.h uiMenuAppendQuitItem"
    c_uiMenuAppendQuitItem :: CUIMenu -> IO CUIMenuItem

foreign import capi "ui.h uiMenuAppendPreferencesItem"
    c_uiMenuAppendPreferencesItem :: CUIMenu -> IO CUIMenuItem

foreign import capi "ui.h uiMenuAppendAboutItem"
    c_uiMenuAppendAboutItem :: CUIMenu -> IO CUIMenuItem

foreign import capi "ui.h uiMenuAppendSeparator"
    c_uiMenuAppendSeparator :: CUIMenu -> IO ()

foreign import capi "ui.h uiNewMenu"
    c_uiNewMenu :: CString -> IO CUIMenu

-- * UI alerts
foreign import capi "ui.h uiOpenFile"
    c_uiOpenFile :: CUIWindow -> IO CString

foreign import capi "ui.h uiSaveFile"
    c_uiSaveFile :: CUIWindow -> IO CString

foreign import capi "ui.h uiMsgBox"
    c_uiMsgBox :: CUIWindow -> CString -> CString -> IO ()

foreign import capi "ui.h uiMsgBoxError"
    c_uiMsgBoxError :: CUIWindow -> CString -> CString -> IO ()
-- tring -> CString -> IO ()

-- foreign import capi "ui.h uiMsgBoxError"
--     c_uiMsgBoxError :: CUIWindow -> CString -> CString -> IO ()
