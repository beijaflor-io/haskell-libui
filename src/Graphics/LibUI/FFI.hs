{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI         #-}
-- |
-- Provides a raw Haskell C FFI interface with libui
-- This is fully untyped, passing void pointers everywhere.
module Graphics.LibUI.FFI
  where

import           Foreign   hiding (void)
import           Foreign.C

-- * Basic API

-- |
-- At the moment the FFI doesn't care about any of the types.
type VoidPtr = Ptr ()

-- |
-- We use type-aliases to make it easier to change in the future.
--
-- The libui library also casts them away; that's why we ignore them for now.
--
-- 'CUIControl' is a pointer to some control we can display
type CUIControl = VoidPtr

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

-- |
-- Display a CUIControl pointer
foreign import capi "ui.h uiControlShow"
    c_uiControlShow :: CUIControl -> IO ()

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
type CUIWindow = VoidPtr

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
    c_uiWindowSetChild :: VoidPtr -> VoidPtr -> IO ()

-- | Is the window margined
foreign import capi "ui.h uiWindowMargined"
    c_uiWindowMargined :: VoidPtr -> IO CInt

-- | Make the window margined
foreign import capi "ui.h uiWindowSetMargined"
    c_uiWindowSetMargined :: VoidPtr -> CInt -> IO ()

-- | Create a new window
foreign import capi "ui.h uiNewWindow"
    c_uiNewWindow :: CString -> CInt -> CInt -> CInt -> IO CUIWindow

-- ** Buttons
-- *** CUIButton <- uiButton
type CUIButton = VoidPtr

foreign import capi "ui.h uiButtonOnClicked"
    c_uiButtonOnClicked :: CUIButton -> FunPtr (CUIButton -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiButtonSetText"
    c_uiButtonSetText :: CUIButton -> CString -> IO ()

foreign import capi "ui.h uiButtonText"
    c_uiButtonText :: CUIButton -> CString

foreign import capi "ui.h uiNewButton"
    c_uiNewButton :: CString -> IO CUIButton

-- ** Layout
-- *** CUIBox <- uiBox
type CUIBox = VoidPtr

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
type CUICheckbox = VoidPtr

foreign import capi "ui.h uiCheckboxText"
    c_uiCheckboxText :: CUICheckbox -> IO CString

foreign import capi "ui.h uiCheckboxSetText"
    c_uiCheckboxSetText :: CUICheckbox -> CString -> IO ()

foreign import capi "ui.h uiCheckboxOnToggled"
    c_uiCheckboxOnToggled :: CUICheckbox -> FunPtr (CUICheckbox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiCheckboxChecked"
    c_uiCheckboxChecked :: CUICheckbox -> IO CInt

foreign import capi "ui.h uiCheckboxSetChecked"
    c_uiCheckboxSetChecked :: CUICheckbox -> CInt -> IO ()

foreign import capi "ui.h uiNewCheckbox"
    c_uiNewCheckbox :: CString -> IO CUICheckbox

-- *** CUIEntry <- uiEntry
type CUIEntry = VoidPtr
foreign import capi "ui.h uiEntryText"
    c_uiEntryText :: CUIEntry -> IO CString

foreign import capi "ui.h uiEntrySetText"
    c_uiEntrySetText :: CUIEntry -> CString -> IO ()

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
type CUILabel = VoidPtr
foreign import capi "ui.h uiLabelText"
    c_uiLabelText :: CUILabel -> IO CString

foreign import capi "ui.h uiLabelSetText"
    c_uiLabelSetText :: CUILabel -> CString -> IO ()

foreign import capi "ui.h uiNewLabel"
    c_uiNewLabel :: CString -> IO CUILabel

-- ** CUITab <- uiTab
type CUITab = VoidPtr
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
type CUIGroup = VoidPtr
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
type CUISpinbox = VoidPtr
foreign import capi "ui.h uiSpinboxValue"
    c_uiSpinboxValue :: CUISpinbox -> IO CInt

foreign import capi "ui.h uiSpinboxSetValue"
    c_uiSpinboxSetValue :: CUISpinbox -> CInt -> IO ()

foreign import capi "ui.h uiSpinboxOnChanged"
    c_uiSpinboxOnChanged :: CUISpinbox -> FunPtr (CUISpinbox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewSpinbox"
    c_uiNewSpinbox :: CInt -> CInt -> IO CUISpinbox

-- ** CUISlider <- uiSlider
type CUISlider = VoidPtr
foreign import capi "ui.h uiSliderValue"
    c_uiSliderValue :: CUISlider -> IO CInt

foreign import capi "ui.h uiSliderSetValue"
    c_uiSliderSetValue :: CUISlider -> CInt -> IO ()

foreign import capi "ui.h uiSliderOnChanged"
    c_uiSliderOnChanged :: CUISlider -> FunPtr (CUISlider -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewSlider"
    c_uiNewSlider :: CInt -> CInt -> IO CUISlider

-- ** CUIProgressBar <- uiProgressBar
type CUIProgressBar = VoidPtr
foreign import capi "ui.h uiProgressBarValue"
    c_uiProgressBarValue :: CUIProgressBar -> IO CInt

foreign import capi "ui.h uiProgressBarSetValue"
    c_uiProgressBarSetValue :: CUIProgressBar -> CInt -> IO ()

foreign import capi "ui.h uiNewProgressBar"
    c_uiNewProgressBar :: IO CUIProgressBar

-- ** CUISeparator <- uiSeparator
type CUISeparator = VoidPtr
foreign import capi "ui.h uiNewHorizontalSeparator"
    c_uiNewHorizontalSeparator :: IO CUISeparator

foreign import capi "ui.h uiNewVerticalSeparator"
    c_uiNewVerticalSeparator :: IO CUISeparator

-- ** CUICombobox <- uiCombobox
type CUICombobox = VoidPtr
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
type CUIEditableCombobox = VoidPtr
foreign import capi "ui.h uiEditableComboboxAppend"
    c_uiEditableComboboxAppend :: CUIEditableCombobox -> CUIControl -> IO ()

foreign import capi "ui.h uiEditableComboboxText"
    c_uiEditableComboboxText :: CUIEditableCombobox -> IO CString

foreign import capi "ui.h uiEditableComboboxSetText"
    c_uiEditableComboboxSetText :: CUIEditableCombobox -> CString -> IO ()

foreign import capi "ui.h uiEditableComboboxOnChanged"
    c_uiEditableComboboxOnChanged :: CUIEditableCombobox -> FunPtr (CUIEditableCombobox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiNewEditableCombobox"
    c_uiNewEditableCombobox :: IO CUIEditableCombobox

-- ** CUIRadioButtons <- uiRadioButtons
type CUIRadioButtons = VoidPtr
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
type CUIForm = VoidPtr
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
type CUIMultilineEntry = VoidPtr
foreign import capi "ui.h uiMultilineEntryText"
    c_uiMultilineEntryText :: CUIMultilineEntry -> IO CString

foreign import capi "ui.h uiMultilineEntrySetText"
    c_uiMultilineEntrySetText :: CUIMultilineEntry -> CString -> IO ()

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
type CUIMenuItem = VoidPtr
foreign import capi "ui.h uiMenuItemEnable"
    c_uiMenuItemEnable :: CUIMenuItem -> IO ()

foreign import capi "ui.h uiMenuItemDisable"
    c_uiMenuItemDisable :: CUIMenuItem -> IO ()

foreign import capi "ui.h uiMenuItemOnClicked"
    c_uiMenuItemOnClicked :: CUIMenuItem -> FunPtr (CUIMenuItem -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import capi "ui.h uiMenuItemChecked"
    c_uiMenuItemChecked :: CUIMenuItem -> IO CInt

foreign import capi "ui.h uiMenuItemSetChecked"
    c_uiMenuItemSetChecked :: CUIMenuItem -> CInt -> IO ()

-- ** CUIMenu <- uiMenu
type CUIMenu = VoidPtr
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
