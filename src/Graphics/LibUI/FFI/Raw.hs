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
-- |
-- Provides a raw Haskell C FFI to libui
--
-- All functions and newtype pointer wrappers imported from the library are
-- prefixed with @CUI...@ or @c_...@
--
-- You can use this module if you want to have access to the C API or write an
-- efficient abstraction layer. For most of the cases, using the
-- 'Graphics.LibUI.FFI' module is a better idea.
--
-- It wraps every function here to make sense within Haskell semantics, namely:
-- respecting that threads may be interrupted asynchronously by exceptions
-- (including the "C" main loop), making it easier to construct controls and
-- combine them and writting code that isn't dependend on the control's type.
module Graphics.LibUI.FFI.Raw
  where

import           Foreign
import           Foreign.C
import           Foreign.CStorable
import           GHC.Generics

-- * Basic API

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
-- Queue action on the UI thread (multi-threaded needs this)
foreign import ccall interruptible "uiQueueMain"
    c_uiQueueMain :: FunPtr (DataPtr -> IO ()) -> DataPtr -> IO ()

-- |
-- Callback for when the application is going to quit
foreign import capi "ui.h uiOnShouldQuit"
    c_uiOnShouldQuit :: FunPtr (DataPtr -> IO CInt) -> DataPtr -> IO ()


-- ** CUIControl
-- libui is decently object-oriented though written in C
--
-- All objects are subclasses of uiControl and casted to it for general
-- operations; we get a similar interface with type-safety

-- |
-- 'CUIControl' is a `uiControl`
--
-- Everything displayed on the UI should be convertible to a 'CUIControl'. It
-- gives us access to UI tree building functions.
newtype CUIControl = CUIControl (Ptr ())

-- |
-- Something that we can convert to a 'CUIControl' and use it's functions
--
-- Default instances are generated for all imported pointer types, because
-- they're all castable to 'CUIControl'
class ToCUIControl a where
    toCUIControl :: a -> CUIControl

instance ToCUIControl CUIControl where
    toCUIControl = id

instance ToCUIControl (Ptr a) where
    toCUIControl = CUIControl . castPtr

-- |
-- Something that we can convert to a 'CUIControl', but need IO
class ToCUIControlIO a where
    -- type CUIType a -- :: ToCUIControl b => b
    toCUIControlIO :: a -> IO CUIControl

class ToCUIControl b => ToCUIControlIO' a b where
    toCUIIO :: ToCUIControl b => a -> IO b

instance ToCUIControl a => ToCUIControlIO' a CUIControl where
    toCUIIO x = return $ toCUIControl x

instance ToCUIControlIO' a CUIControl => ToCUIControlIO a where
    toCUIControlIO = toCUIIO

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

-- * UI Controls
-- ** Windows
-- *** CUIWindow <- uiWindow

-- |
-- A C window
--
-- @
-- -- | An action that creates a window with a child
-- main = do
--     uiInit
--     let title = "Hello World"
--         width = 680
--         height = 400
--         hasMenubar = True
--     win <- 'uiNewWindow' title width height hasMenubar
--     -- ^ Get hold of the window pointer
--     lbl <- 'uiNewLabel'
--     win ``setChild`` lbl
--     -- ^ Add a label as a child of the window
--     uiShow win
--     uiMain
-- @
newtype CUIWindow = CUIWindow (Ptr RawWindow)
  deriving(Show, ToCUIControl)

data RawWindow

-- | Get the window title
foreign import capi "ui.h uiWindowTitle"
    c_uiWindowTitle :: CUIWindow -> IO CString

-- | Set the window title
foreign import capi "ui.h uiWindowSetTitle"
    c_uiWindowSetTitle
        :: CUIWindow
        -> CString
        -- ^ A new title
        -> IO ()

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
    c_uiWindowOnContentSizeChanged :: CUIWindow -> FunPtr (CUIWindow -> DataPtr -> IO ()) -> DataPtr -> IO ()

-- | Add a callback to when the window is closed
foreign import capi "ui.h uiWindowOnClosing"
    c_uiWindowOnClosing :: CUIWindow -> FunPtr (CUIWindow -> DataPtr -> IO ()) -> DataPtr -> IO ()

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
    c_uiNewWindow
      :: CString
      -- ^ The window title
      -> CInt
      -- ^ The window width
      -> CInt
      -- ^ The window height
      -> CInt
      -- ^ Whether the window has a menubar
      -> IO CUIWindow

-- ** Labels
-- *** CUILabel <- uiLabel
-- | A text label, which can currently only span one line
--
-- @
-- -- ...
-- lbl <- uiNewLabel "Before 10 seconds"
-- forkIO $ do
--     threadDelay (1000 * 1000 * 10)
--     uiQueueMain $ lbl `setText` "After 10 seconds"
-- -- ...
-- @
newtype CUILabel = CUILabel (Ptr RawLabel)
  deriving(Show, ToCUIControl)
data RawLabel

foreign import capi "ui.h uiLabelText"
    c_uiLabelText :: CUILabel -> IO CString

foreign import capi "ui.h uiLabelSetText"
    c_uiLabelSetText :: CUILabel -> CString -> IO ()

foreign import capi "ui.h uiNewLabel"
    c_uiNewLabel :: CString -> IO CUILabel

-- ** Layout
-- *** CUIBox <- uiBox
-- | Either a vertical or horizontal box of items
--
-- @
-- -- ...
-- hbox <- uiNewHorizontalBox
-- btn <- uiNewButton "Click me"
-- hbox `setPadded` True
-- hbox `appendChild` btn
-- hbox `appendChildIO` uiNewLabel "Label"
-- -- ...
-- @
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

-- *** CUITabs <- uiTab
-- | Tabbed Interface
--
-- @
-- -- ...
-- tabs <- uiNewTabs
-- tabs `appendTab` ("Some other control", c1)
-- tabs `appendTabMargined` ("Some other control with a margin", c2)
-- -- ...
-- @
newtype CUITabs = CUITab (Ptr RawTab)
  deriving(Show, ToCUIControl)
data RawTab

foreign import capi "ui.h uiTabAppend"
    c_uiTabAppend
      :: CUITabs
      -- ^ The tab group pointer
      -> CString
      -- ^ The tab title
      -> CUIControl
      -- ^ The tab contents
      -> IO ()

foreign import capi "ui.h uiTabInsertAt"
    c_uiTabInsertAt :: CUITabs -> CString -> CInt -> CUIControl -> IO ()

foreign import capi "ui.h uiTabDelete"
    c_uiTabDelete :: CUITabs -> CInt -> IO ()

foreign import capi "ui.h uiTabNumPages"
    c_uiTabNumPages :: CUITabs -> IO CInt

foreign import capi "ui.h uiTabMargined"
    c_uiTabMargined :: CUITabs -> CInt -> IO CInt

foreign import capi "ui.h uiTabSetMargined"
    c_uiTabSetMargined :: CUITabs -> CInt -> CInt -> IO ()

foreign import capi "ui.h uiNewTab"
    c_uiNewTab :: IO CUITabs

-- *** CUIGroup <- uiGroup
-- | Named Control Groups
--
-- @
-- -- ...
-- group <- uiNewGroup "Group Name"
-- group `setMargined` True
-- group `setChild` c1
-- -- ...
-- @
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

-- *** CUIGrid <- uiGrid
-- | Layout Grids
newtype CUIGrid = CUIGrid (Ptr RawGrid)
  deriving(Show, ToCUIControl)
data RawGrid

newtype CUIAlign = CUIAlign CInt
  deriving(Show)

newtype CUIAt = CUIAt CInt
  deriving(Show)

foreign import capi "ui.h uiGridAppend"
    c_uiGridAppend
        :: CUIGrid
        -> CUIControl
        -> CInt
        -- ^ Left
        -> CInt
        -- ^ Top
        -> CInt
        -- ^ Xspan
        -> CInt
        -- ^ Yspan
        -> CInt
        -- ^ Hexpand
        -> CUIAlign
        -- ^ Halign
        -> CInt
        -- ^ Vexpand
        -> CUIAlign
        -- ^ Valign
        -> IO ()

foreign import capi "ui.h uiGridInsertAt"
    c_uiGridInsertAt
        :: CUIGrid
        -> CUIControl
        -- ^ New control
        -> CUIControl
        -- ^ Old, existing control
        -> CUIAt
        -- ^ At
        -> CInt
        -- ^ Xspan
        -> CInt
        -- ^ Yspan
        -> CInt
        -- ^ Hexpand
        -> CUIAlign
        -- ^ Halign
        -> CInt
        -- ^ Vexpand
        -> CUIAlign
        -- ^ Valign
        -> IO ()

foreign import capi "ui.h uiGridPadded"
    c_uiGridPadded :: CUIGrid -> IO CInt

foreign import capi "ui.h uiGridSetPadded"
    c_uiGridSetPadded :: CUIGrid -> CInt -> IO ()

foreign import capi "ui.h uiNewGrid"
    c_uiNewGrid :: IO CUIGrid

-- *** CUISeparator <- uiSeparator
-- | Separators
newtype CUISeparator = CUISeparator (Ptr RawSeparator)
  deriving(Show, ToCUIControl)
data RawSeparator

foreign import capi "ui.h uiNewHorizontalSeparator"
    c_uiNewHorizontalSeparator :: IO CUISeparator

foreign import capi "ui.h uiNewVerticalSeparator"
    c_uiNewVerticalSeparator :: IO CUISeparator

-- ** Input Types
-- *** Buttons
-- **** CUIButton <- uiButton

-- |
-- Buttons
--
-- @
-- import Graphics.LibUI.FFI
-- makeMyButton :: IO 'CUIButton'
-- makeMyButton = do
--     btn <- 'uiNewButton' "Hello world"
--     -- ^ Get hold of the button pointer
--     btn ``onClick`` print "Clicked!"
--     -- ^ Add a 'onClick' handler to the control
--     return btn
--     -- ^ Return the pointer for later use
-- @
newtype CUIButton = CUIButton (Ptr RawButton)
  deriving(Show, ToCUIControl)
data RawButton

foreign import capi "ui.h uiButtonOnClicked"
    c_uiButtonOnClicked :: CUIButton -> FunPtr (CUIButton -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiButtonSetText"
    c_uiButtonSetText :: CUIButton -> CString -> IO ()

foreign import capi "ui.h uiButtonText"
    c_uiButtonText :: CUIButton -> IO CString

foreign import capi "ui.h uiNewButton"
    c_uiNewButton :: CString -> IO CUIButton

-- *** CUICheckbox <- uiCheckbox
-- | A checkbox
newtype CUICheckbox = CUICheckbox (Ptr RawCheckbox)
  deriving(Show, ToCUIControl)
data RawCheckbox

foreign import capi "ui.h uiCheckboxText"
    c_uiCheckboxText :: CUICheckbox -> IO CString

foreign import capi "ui.h uiCheckboxSetText"
    c_uiCheckboxSetText :: CUICheckbox -> CString -> IO ()

foreign import capi "ui.h uiCheckboxOnToggled"
    c_uiCheckboxOnToggled :: CUICheckbox -> FunPtr (CUICheckbox -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiCheckboxChecked"
    c_uiCheckboxChecked :: CUICheckbox -> IO CInt

foreign import capi "ui.h uiCheckboxSetChecked"
    c_uiCheckboxSetChecked :: CUICheckbox -> CInt -> IO ()

foreign import capi "ui.h uiNewCheckbox"
    c_uiNewCheckbox :: CString -> IO CUICheckbox

-- *** CUIEntry <- uiEntry
-- | A text input
newtype CUIEntry = CUIEntry (Ptr RawEntry)
  deriving(Show, ToCUIControl)
data RawEntry

foreign import capi "ui.h uiEntryText"
    c_uiEntryText :: CUIEntry -> IO CString

foreign import capi "ui.h uiEntrySetText"
    c_uiEntrySetText :: CUIEntry -> CString -> IO ()

foreign import capi "ui.h uiEntryOnChanged"
    c_uiEntryOnChanged :: CUIEntry -> FunPtr (CUIEntry -> DataPtr -> IO ()) -> DataPtr -> IO ()

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

-- *** CUISlider <- uiSlider
-- | A range slider
newtype CUISlider = CUISlider (Ptr RawSlider)
  deriving(Show, ToCUIControl)
data RawSlider

foreign import capi "ui.h uiSliderValue"
    c_uiSliderValue :: CUISlider -> IO CInt

foreign import capi "ui.h uiSliderSetValue"
    c_uiSliderSetValue :: CUISlider -> CInt -> IO ()

foreign import capi "ui.h uiSliderOnChanged"
    c_uiSliderOnChanged :: CUISlider -> FunPtr (CUISlider -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewSlider"
    c_uiNewSlider :: CInt -> CInt -> IO CUISlider

-- *** CUICombobox <- uiCombobox
-- | A select input
newtype CUICombobox = CUICombobox (Ptr RawCombobox)
  deriving(Show, ToCUIControl)
data RawCombobox

foreign import capi "ui.h uiComboboxAppend"
    c_uiComboboxAppend :: CUICombobox -> CString -> IO ()

foreign import capi "ui.h uiComboboxSelected"
    c_uiComboboxSelected :: CUICombobox -> IO CInt

foreign import capi "ui.h uiComboboxSetSelected"
    c_uiComboboxSetSelected :: CUICombobox -> CInt -> IO ()

foreign import capi "ui.h uiComboboxOnSelected"
    c_uiComboboxOnSelected :: CUICombobox -> FunPtr (CUICombobox -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewCombobox"
    c_uiNewCombobox :: IO CUICombobox

-- *** CUIEditableCombobox <- uiEditableCombobox
-- | An editable select input
newtype CUIEditableCombobox = CUIEditableCombobox (Ptr RawEditableCombobox)
  deriving(Show, ToCUIControl)
data RawEditableCombobox

foreign import capi "ui.h uiEditableComboboxAppend"
    c_uiEditableComboboxAppend :: CUIEditableCombobox -> CString -> IO ()

foreign import capi "ui.h uiEditableComboboxText"
    c_uiEditableComboboxText :: CUIEditableCombobox -> IO CString

foreign import capi "ui.h uiEditableComboboxSetText"
    c_uiEditableComboboxSetText :: CUIEditableCombobox -> CString -> IO ()

foreign import capi "ui.h uiEditableComboboxOnChanged"
    c_uiEditableComboboxOnChanged :: CUIEditableCombobox -> FunPtr (CUIEditableCombobox -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewEditableCombobox"
    c_uiNewEditableCombobox :: IO CUIEditableCombobox

-- *** CUIRadioButtons <- uiRadioButtons
-- | Radio buttons
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
    c_uiRadioButtonsOnSelected :: CUIRadioButtons -> FunPtr (CUIRadioButtons -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewRadioButtons"
    c_uiNewRadioButtons :: IO CUIRadioButtons

-- *** CUIForm <- uiForm
-- | Forms (groups of labeled inputs)
--
-- @
-- -- ...
-- form <- uiNewForm
-- form `appendInput` ("Name", inp1) -- <- this function is polymorphic
-- form `setPadded` True
-- -- ...
-- @
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

-- *** CUIDatePicker <- uiDatePicker
newtype CUIDateTimePicker = CUIDateTimePicker (Ptr RawDateTimePicker)
  deriving(Show, ToCUIControl)
data RawDateTimePicker

foreign import capi "ui.h uiNewDatePicker"
    c_uiNewDatePicker :: IO CUIDateTimePicker

foreign import capi "ui.h uiNewTimePicker"
    c_uiNewTimePicker :: IO CUIDateTimePicker

foreign import capi "ui.h uiNewDateTimePicker"
    c_uiNewDateTimePicker :: IO CUIDateTimePicker

-- *** CUIFontButton <- uiFontButton
newtype CUIFontButton = CUIFontButton (Ptr RawFontButton)
  deriving(Show, ToCUIControl)
data RawFontButton

foreign import capi "ui.h uiNewFontButton"
    c_uiNewFontButton :: IO CUIFontButton

-- *** CUIColorButton <- uiColorButton
newtype CUIColorButton = CUIColorButton (Ptr RawColorButton)
  deriving(Show, ToCUIControl)
data RawColorButton

foreign import capi "ui.h uiNewColorButton"
    c_uiNewColorButton :: IO CUIColorButton

-- *** CUIMultilineEntry <- uiMultilineEntry
newtype CUIMultilineEntry = CUIMultilineEntry (Ptr RawMultilineEntry)
  deriving(Show, ToCUIControl)
data RawMultilineEntry

foreign import capi "ui.h uiMultilineEntryText"
    c_uiMultilineEntryText :: CUIMultilineEntry -> IO CString

foreign import capi "ui.h uiMultilineEntrySetText"
    c_uiMultilineEntrySetText :: CUIMultilineEntry -> CString -> IO ()

foreign import capi "ui.h uiMultilineEntryAppend"
    c_uiMultilineEntryAppend :: CUIMultilineEntry -> CString -> IO ()

foreign import capi "ui.h uiMultilineEntryOnChanged"
    c_uiMultilineEntryOnChanged :: CUIMultilineEntry -> FunPtr (CUIMultilineEntry -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiMultilineEntryReadOnly"
    c_uiMultilineEntryReadOnly :: CUIMultilineEntry -> IO CInt

foreign import capi "ui.h uiMultilineEntrySetReadOnly"
    c_uiMultilineEntrySetReadOnly :: CUIMultilineEntry -> CInt -> IO ()

foreign import capi "ui.h uiNewMultilineEntry"
    c_uiNewMultilineEntry :: IO CUIMultilineEntry

foreign import capi "ui.h uiNewNonWrappingMultilineEntry"
    c_uiNewNonWrappingMultilineEntry :: IO CUIMultilineEntry

-- ** Progress Indicators
-- *** CUIProgressBar <- uiProgressBar
newtype CUIProgressBar = CUIProgressBar (Ptr RawProgressBar)
  deriving(Show, ToCUIControl)
data RawProgressBar

foreign import capi "ui.h uiProgressBarValue"
    c_uiProgressBarValue :: CUIProgressBar -> IO CInt

foreign import ccall safe "uiProgressBarSetValue"
    c_uiProgressBarSetValue :: CUIProgressBar -> CInt -> IO ()

foreign import capi "ui.h uiNewProgressBar"
    c_uiNewProgressBar :: IO CUIProgressBar

-- *** CUISpinbox <- uiSpinbox
newtype CUISpinbox = CUISpinbox (Ptr RawSpinbox)
  deriving(Show, ToCUIControl)
data RawSpinbox

foreign import capi "ui.h uiSpinboxValue"
    c_uiSpinboxValue :: CUISpinbox -> IO CInt

foreign import capi "ui.h uiSpinboxSetValue"
    c_uiSpinboxSetValue :: CUISpinbox -> CInt -> IO ()

foreign import capi "ui.h uiSpinboxOnChanged"
    c_uiSpinboxOnChanged :: CUISpinbox -> FunPtr (CUISpinbox -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewSpinbox"
    c_uiNewSpinbox :: CInt -> CInt -> IO CUISpinbox

-- * The Menubar
-- ** CUIMenu <- uiMenu

newtype CUIMenu = CUIMenu (Ptr RawMenu)
  deriving(ToCUIControl, Show)
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

-- ** CUIMenuItem <- uiMenuItem
newtype CUIMenuItem = CUIMenuItem (Ptr RawMenuItem)
  deriving(Show)
data RawMenuItem

foreign import capi "ui.h uiMenuItemEnable"
    c_uiMenuItemEnable :: CUIMenuItem -> IO ()

foreign import capi "ui.h uiMenuItemDisable"
    c_uiMenuItemDisable :: CUIMenuItem -> IO ()

foreign import capi "ui.h uiMenuItemOnClicked"
    c_uiMenuItemOnClicked :: CUIMenuItem -> FunPtr (CUIMenuItem -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiMenuItemChecked"
    c_uiMenuItemChecked :: CUIMenuItem -> IO CInt

foreign import capi "ui.h uiMenuItemSetChecked"
    c_uiMenuItemSetChecked :: CUIMenuItem -> CInt -> IO ()

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
  deriving(Show, Generic)

newtype CUIAreaMouseEvent = CUIAreaMouseEvent (Ptr RawAreaMouseEvent)
  deriving(Show)

data RawAreaMouseEvent

data CUIAreaHandler =
    CUIAreaHandler { cuiAreaHandlerDraw :: FunPtr (Ptr CUIAreaHandler -> CUIArea -> Ptr CUIAreaDrawParams -> IO ())
                   , cuiAreaHandlerMouseEvent :: FunPtr (Ptr CUIAreaHandler -> CUIArea -> CUIAreaMouseEvent -> IO ())
                   }
  deriving(Show, Generic)

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

-- * UI Alerts and Dialogs
foreign import capi "ui.h uiOpenFile"
    c_uiOpenFile :: CUIWindow -> IO CString

foreign import capi "ui.h uiSaveFile"
    c_uiSaveFile :: CUIWindow -> IO CString

foreign import capi "ui.h uiMsgBox"
    c_uiMsgBox :: CUIWindow -> CString -> CString -> IO ()

foreign import capi "ui.h uiMsgBoxError"
    c_uiMsgBoxError :: CUIWindow -> CString -> CString -> IO ()

-- * Support API
-- ** Creating callbacks to pass to C and call back to Haskell

-- |
-- The callback API passes around a void pointer, which is state that can be
-- threaded to callbacks. This is not necessary in Haskell land and ignored.
type DataPtr = Ptr ()

-- |
-- Wrap a success callback on a foreign pointer
foreign import ccall "wrapper"
    c_wrap1I :: (DataPtr -> IO CInt) -> IO (FunPtr (DataPtr -> IO CInt))

-- |
-- Wrap a 1 argument event listener on a foreign pointer
foreign import ccall "wrapper"
    c_wrap1 :: (DataPtr -> IO ()) -> IO (FunPtr (DataPtr -> IO ()))

-- |
-- Wrap a 2 argument event listener on a foreign pointer
foreign import ccall "wrapper"
    c_wrap2 :: (DataPtr -> DataPtr -> IO ()) -> IO (FunPtr (DataPtr -> DataPtr -> IO ()))
