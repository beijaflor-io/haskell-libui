{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.LibUI
  where

import           Foreign
import           Foreign.C

type VoidPtr = Ptr ()
type CUIControl = VoidPtr

foreign import ccall "uiMain"
    c_uiMain :: IO ()

foreign import ccall "uiControlShow"
    c_uiControlShow :: CUIControl -> IO ()

foreign import ccall "wrapper"
    c_wrap2 :: (VoidPtr -> VoidPtr -> IO ()) -> IO (FunPtr (VoidPtr -> VoidPtr -> IO ()))

-- CUIWindow <- uiWindow
type CUIWindow = VoidPtr
foreign import ccall "uiWindowTitle"
    c_uiWindowTitle :: CUIWindow -> IO CString

foreign import ccall "uiWindowSetTitle"
    c_uiWindowSetTitle :: CUIWindow -> CString -> IO ()

foreign import ccall "uiWindowPosition"
    c_uiWindowPosition :: CUIWindow -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "uiWindowSetPosition"
    c_uiWindowSetPosition :: CUIWindow -> CInt -> CInt -> IO ()

foreign import ccall "uiWindowCenter"
    c_uiWindowCenter :: CUIWindow -> IO ()

foreign import ccall "uiWindowOnPositionChanged"
    c_uiWindowOnPositionChanged :: CUIWindow -> FunPtr (CUIWindow -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiWindowContentSize"
    c_uiWindowContentSize :: CUIWindow -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "uiWindowSetContentSize"
    c_uiWindowSetContentSize :: CUIWindow -> CInt -> CInt -> IO ()

foreign import ccall "uiWindowFullscreen"
    c_uiWindowFullscreen :: CUIWindow -> IO CInt

foreign import ccall "uiWindowSetFullscreen"
    c_uiWindowSetFullscreen :: CUIWindow -> CInt -> IO ()

foreign import ccall "uiWindowOnContentSizeChanged"
    c_uiWindowOnContentSizeChanged :: CUIWindow -> FunPtr (CUIWindow -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiWindowOnClosing"
    c_uiWindowOnClosing :: CUIWindow -> FunPtr (CUIWindow -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiWindowBorderless"
    c_uiWindowBorderless :: CUIWindow -> IO CInt

foreign import ccall "uiWindowSetBorderless"
    c_uiWindowSetBorderless :: CUIWindow -> CInt -> IO ()

foreign import ccall "uiWindowSetChild"
    c_uiWindowSetChild :: VoidPtr -> VoidPtr -> IO ()

foreign import ccall "uiWindowMargined"
    c_uiWindowMargined :: VoidPtr -> IO CInt

foreign import ccall "uiWindowSetMargined"
    c_uiWindowSetMargined :: VoidPtr -> CInt -> IO ()

foreign import ccall "uiNewWindow"
    c_uiNewWindow :: CString -> CInt -> CInt -> CInt -> IO CUIWindow

-- CUIButton <- uiButton
type CUIButton = VoidPtr

foreign import ccall "uiButtonOnClicked"
    c_uiButtonOnClicked :: CUIButton -> FunPtr (CUIButton -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiButtonSetText"
    c_uiButtonSetText :: CUIButton -> CString -> IO ()

foreign import ccall "uiButtonText"
    c_uiButtonText :: CUIButton -> CString

foreign import ccall "uiNewButton"
    c_uiNewButton :: CString -> IO CUIButton

-- CUIBox <- uiBox
type CUIBox = VoidPtr

foreign import ccall "uiBoxAppend"
    c_uiBoxAppend :: CUIBox -> CUIControl -> CInt -> IO ()

foreign import ccall "uiBoxDelete"
    c_uiBoxDelete :: CUIBox -> CInt -> IO ()

foreign import ccall "uiBoxPadded"
    c_uiBoxPadded :: CUIBox -> IO CInt

foreign import ccall "uiBoxSetPadded"
    c_uiBoxSetPadded :: CUIBox -> CInt -> IO ()

foreign import ccall "uiNewHorizontalBox"
    c_uiNewHorizontalBox :: IO CUIBox

foreign import ccall "uiNewVerticalBox"
    c_uiNewVerticalBox :: IO CUIBox

-- CUICheckbox <- uiCheckbox
type CUICheckbox = VoidPtr

foreign import ccall "uiCheckboxText"
    c_uiCheckboxText :: CUICheckbox -> IO CString

foreign import ccall "uiCheckboxSetText"
    c_uiCheckboxSetText :: CUICheckbox -> CString -> IO ()

foreign import ccall "uiCheckboxOnToggled"
    c_uiCheckboxOnToggled :: CUICheckbox -> FunPtr (CUICheckbox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiCheckboxChecked"
    c_uiCheckboxChecked :: CUICheckbox -> IO CInt

foreign import ccall "uiCheckboxSetChecked"
    c_uiCheckboxSetChecked :: CUICheckbox -> CInt -> IO ()

foreign import ccall "uiNewCheckbox"
    c_uiNewCheckbox :: IO CUICheckbox

-- CUIEntry <- uiEntry
type CUIEntry = VoidPtr
foreign import ccall "uiEntryText"
    c_uiEntryText :: CUIEntry -> IO CString

foreign import ccall "uiEntrySetText"
    c_uiEntrySetText :: CUIEntry -> CString -> IO ()

foreign import ccall "uiEntryOnChanged"
    c_uiEntryOnChanged :: CUIEntry -> FunPtr (CUIEntry -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiEntryReadOnly"
    c_uiEntryReadOnly :: CUIEntry -> IO CInt

foreign import ccall "uiEntrySetReadOnly"
    c_uiEntrySetReadOnly :: CUIEntry -> CInt -> IO ()

foreign import ccall "uiNewEntry"
    c_uiNewEntry :: IO CUIEntry

foreign import ccall "uiNewPasswordEntry"
    c_uiNewPasswordEntry :: IO CUIEntry

foreign import ccall "uiNewSearchEntry"
    c_uiNewSearchEntry :: IO CUIEntry

-- CUILabel <- uiLabel
type CUILabel = VoidPtr
foreign import ccall "uiLabelText"
    c_uiLabelText :: CUILabel -> IO CString

foreign import ccall "uiLabelSetText"
    c_uiLabelSetText :: CUILabel -> CString -> IO ()

foreign import ccall "uiNewLabel"
    c_uiNewLabel :: CString -> IO CUILabel

-- CUITab <- uiTab
type CUITab = VoidPtr
foreign import ccall "uiTabAppend"
    c_uiTabAppend :: CUITab -> CString -> CUIControl -> IO ()

foreign import ccall "uiTabInsertAt"
    c_uiTabInsertAt :: CUITab -> CString -> CInt -> CUIControl -> IO ()

foreign import ccall "uiTabDelete"
    c_uiTabDelete :: CUITab -> CInt -> IO ()

foreign import ccall "uiTabNumPages"
    c_uiTabNumPages :: CUITab -> IO CInt

foreign import ccall "uiTabMargined"
    c_uiTabMargined :: CUITab -> IO CInt

foreign import ccall "uiTabSetMargined"
    c_uiTabSetMargined :: CUITab -> CInt -> IO ()

foreign import ccall "uiNewTab"
    c_uiNewTab :: IO CUITab

-- CUIGroup <- uiGroup
type CUIGroup = VoidPtr
foreign import ccall "uiGroupTitle"
    c_uiGroupTitle :: CUIGroup -> IO CString

foreign import ccall "uiGroupSetTitle"
    c_uiGroupSetTitle :: CUIGroup -> CString -> IO ()

foreign import ccall "uiGroupSetChild"
    c_uiGroupSetChild :: CUIGroup -> CUIControl -> IO ()

foreign import ccall "uiGroupMargined"
    c_uiGroupMargined :: CUIGroup -> IO CInt

foreign import ccall "uiGroupSetMargined"
    c_uiGroupSetMargined :: CUIGroup -> CInt -> IO ()

foreign import ccall "uiNewGroup"
    c_uiNewGroup :: CString -> IO CUIGroup

-- CUISpinbox <- uiSpinbox
type CUISpinbox = VoidPtr
foreign import ccall "uiSpinboxValue"
    c_uiSpinboxValue :: CUISpinbox -> IO CInt

foreign import ccall "uiSpinboxSetValue"
    c_uiSpinboxSetValue :: CUISpinbox -> CInt -> IO ()

foreign import ccall "uiSpinboxOnChanged"
    c_uiSpinboxOnChanged :: CUISpinbox -> FunPtr (CUISpinbox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiNewSpinbox"
    c_uiNewSpinbox :: CInt -> CInt -> IO CUISpinbox

-- CUISlider <- uiSlider
type CUISlider = VoidPtr
foreign import ccall "uiSliderValue"
    c_uiSliderValue :: CUISlider -> IO CInt

foreign import ccall "uiSliderSetValue"
    c_uiSliderSetValue :: CUISlider -> CInt -> IO ()

foreign import ccall "uiSliderOnChanged"
    c_uiSliderOnChanged :: CUISlider -> FunPtr (CUISlider -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiNewSlider"
    c_uiNewSlider :: CInt -> CInt -> IO CUISlider

-- CUIProgressBar <- uiProgressBar
type CUIProgressBar = VoidPtr
foreign import ccall "uiProgressBarValue"
    c_uiProgressBarValue :: CUIProgressBar -> IO CInt

foreign import ccall "uiProgressBarSetValue"
    c_uiProgressBarSetValue :: CUIProgressBar -> CInt -> IO ()

foreign import ccall "uiNewProgressBar"
    c_uiNewProgressBar :: IO CUIProgressBar

-- CUISeparator <- uiSeparator
type CUISeparator = VoidPtr
foreign import ccall "uiNewHorizontalSeparator"
    c_uiNewHorizontalSeparator :: IO CUISeparator

foreign import ccall "uiNewVerticalSeparator"
    c_uiNewVerticalSeparator :: IO CUISeparator

-- CUICombobox <- uiCombobox
type CUICombobox = VoidPtr
foreign import ccall "uiComboboxAppend"
    c_uiComboboxAppend :: CUICombobox -> CUIControl -> IO ()

foreign import ccall "uiComboboxSelected"
    c_uiComboboxSelected :: CUICombobox -> IO CInt

foreign import ccall "uiComboboxSetSelected"
    c_uiComboboxSetSelected :: CUICombobox -> CInt -> IO ()

foreign import ccall "uiComboboxOnSelected"
    c_uiComboboxOnSelected :: CUICombobox -> FunPtr (CUICombobox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiNewCombobox"
    c_uiNewCombobox :: IO CUICombobox

-- CUIEditableCombobox <- uiEditableCombobox
type CUIEditableCombobox = VoidPtr
foreign import ccall "uiEditableComboboxAppend"
    c_uiEditableComboboxAppend :: CUIEditableCombobox -> CUIControl -> IO ()

foreign import ccall "uiEditableComboboxText"
    c_uiEditableComboboxText :: CUIEditableCombobox -> IO CString

foreign import ccall "uiEditableComboboxSetText"
    c_uiEditableComboboxSetText :: CUIEditableCombobox -> CString -> IO ()

foreign import ccall "uiEditableComboboxOnChanged"
    c_uiEditableComboboxOnChanged :: CUIEditableCombobox -> FunPtr (CUIEditableCombobox -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiNewEditableCombobox"
    c_uiNewEditableCombobox :: IO CUIEditableCombobox

-- CUIRadioButtons <- uiRadioButtons
type CUIRadioButtons = VoidPtr
foreign import ccall "uiRadioButtonsAppend"
    c_uiRadioButtonsAppend :: CUIRadioButtons -> CString -> IO ()

foreign import ccall "uiRadioButtonsSelected"
    c_uiRadioButtonsSelected :: CUIRadioButtons -> IO CInt

foreign import ccall "uiRadioButtonsSetSelected"
    c_uiRadioButtonsSetSelected :: CUIRadioButtons -> CInt -> IO ()

foreign import ccall "uiRadioButtonsOnSelected"
    c_uiRadioButtonsOnSelected :: CUIRadioButtons -> FunPtr (CUIRadioButtons -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiNewRadioButtons"
    c_uiNewRadioButtons :: IO CUIRadioButtons

-- CUIMultilineEntry <- uiMultilineEntry
type CUIMultilineEntry = VoidPtr
foreign import ccall "uiMultilineEntryText"
    c_uiMultilineEntryText :: CUIMultilineEntry -> IO CString

foreign import ccall "uiMultilineEntrySetText"
    c_uiMultilineEntrySetText :: CUIMultilineEntry -> CString -> IO ()

foreign import ccall "uiMultilineEntryAppend"
    c_uiMultilineEntryAppend :: CUIMultilineEntry -> CString -> IO ()

foreign import ccall "uiMultilineEntryOnChanged"
    c_uiMultilineEntryOnChanged :: CUIMultilineEntry -> FunPtr (CUIMultilineEntry -> VoidPtr -> IO ()) -> VoidPtr -> IO ()

foreign import ccall "uiMultilineEntryReadOnly"
    c_uiMultilineEntryReadOnly :: CUIMultilineEntry -> IO CInt

foreign import ccall "uiMultilineEntrySetReadOnly"
    c_uiMultilineEntrySetReadOnly :: CUIMultilineEntry -> CInt -> IO ()

foreign import ccall "uiNewMultilineEntry"
    c_uiNewMultilineEntry :: IO CUIMultilineEntry

foreign import ccall "uiNewNonWrappingMultilineEntry"
    c_uiNewNonWrappingMultilineEntry :: IO CUIMultilineEntry

-- CUIMenuItem <- uiMenuItem
type CUIMenuItem = VoidPtr
foreign import ccall "uiMenuItemEnable"
    c_uiMenuItemEnable :: CUIMenuItem -> IO ()

foreign import ccall "uiMenuItemDisable"
    c_uiMenuItemDisable :: CUIMenuItem -> IO ()

foreign import ccall "uiMenuItemOnClicked"
    c_uiMenuItemOnClicked :: CUIMenuItem -> FunPtr (CUIMenuItem -> VoidPtr -> IO ()) -> IO ()

foreign import ccall "uiMenuItemChecked"
    c_uiMenuItemChecked :: CUIMenuItem -> IO CInt

foreign import ccall "uiMenuItemSetChecked"
    c_uiMenuItemSetChecked :: CUIMenuItem -> CInt -> IO ()

-- CUIMenu <- uiMenu
type CUIMenu = VoidPtr
foreign import ccall "uiMenuAppendItem"
    c_uiMenuAppendItem :: CUIMenu -> CString -> IO CUIMenuItem

foreign import ccall "uiMenuAppendCheckItem"
    c_uiMenuAppendCheckItem :: CUIMenu -> CString -> IO CUIMenuItem

foreign import ccall "uiMenuAppendQuitItem"
    c_uiMenuAppendQuitItem :: CUIMenu -> IO CUIMenuItem

foreign import ccall "uiMenuAppendPreferencesItem"
    c_uiMenuAppendPreferencesItem :: CUIMenu -> IO CUIMenuItem

foreign import ccall "uiMenuAppendAboutItem"
    c_uiMenuAppendAboutItem :: CUIMenu -> IO CUIMenuItem

foreign import ccall "uiMenuAppendSeparator"
    c_uiMenuAppendSeparator :: CUIMenu -> IO ()

foreign import ccall "uiNewMenu"
    c_uiNewMenu :: IO CUIMenu

-- UI alerts
foreign import ccall "uiOpenFile"
    c_uiOpenFile :: CUIWindow -> IO CString

foreign import ccall "uiSaveFile"
    c_uiSaveFile :: CUIWindow -> IO CString

foreign import ccall "uiMsgBox"
    c_uiMsgBox :: CUIWindow -> CString -> CString -> IO ()

foreign import ccall "uiMsgBoxError"
    c_uiMsgBoxError :: CUIWindow -> CString -> CString -> IO ()
