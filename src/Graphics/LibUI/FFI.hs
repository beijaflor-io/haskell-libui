{-# LANGUAGE CApiFFI                    #-}
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
-- Provides a raw Haskell C FFI interface with libui as well as type-classes
-- and Haskell wrappers to use them
--
-- This module should be enough to match how most imperative languages will
-- work with the foreign library, if you're ok with building your GUI
-- imperatively on the IO Monad, this should be fine
--
-- All functions and newtype pointer wrappers imported from the library are
-- prefixed with @CUI...@ or @c_...@
module Graphics.LibUI.FFI
  where

import           Control.Monad ((>=>))
import           Foreign       hiding (void)
import           Foreign.C

-- * Basic API

-- |
-- The callback API passes around a void pointer, which is state that can be
-- threaded to callbacks. This is not necessary in Haskell land and ignored.
type DataPtr = Ptr ()

boolToNum :: Num a => Bool -> a
boolToNum False = 0
boolToNum True = 1

numToBool :: (Num a, Eq a) => a -> Bool
numToBool 0 = False
numToBool _ = True

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
uiInit :: IO ()
uiInit = alloca $ \ptr -> do
    poke ptr (CSize (fromIntegral (sizeOf (CSize 0))))
    c_uiInit ptr

foreign import capi "ui.h uiInit"
    c_uiInit :: Ptr CSize -> IO ()

foreign import capi "ui.h uiQueueMain"
    c_uiQueueMain :: FunPtr (DataPtr -> IO ()) -> DataPtr -> IO ()

-- |
-- Initialize the UI
foreign import capi "ui.h uiOnShouldQuit"
    c_uiOnShouldQuit :: FunPtr (DataPtr -> IO CInt) -> DataPtr -> IO ()

-- ** CUIControl
-- libui is decently object-oriented though written in C
--
-- All objects are subclasses of uiControl and casted to it for general
-- operations; we get a similar interface with type-safety

-- |
-- 'CUIControl' is a `uiControl`
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
    toCUIControlIO :: a -> IO CUIControl

instance ToCUIControl a => ToCUIControlIO a where
    toCUIControlIO = return . toCUIControl

-- |
-- Controls with `ui...SetText` functions
class HasSetText s where
    setText :: s -> String -> IO ()

-- |
-- Controls with `ui...SetValue` functions
class HasSetValue s where
    setValue :: s -> Int -> IO ()

-- |
-- Controls with `ui...GetValue` functions
class HasGetValue s where
    getValue :: s -> IO Int

-- |
-- Controls with `ui...OnClicked` functions
class HasOnClicked s where
    onClick :: s -> IO () -> IO ()

-- |
-- Controls with `ui...SetChecked` functions
class HasSetChecked s where
    setChecked :: s -> Bool -> IO ()

-- |
-- Controls with `ui...SetChild` functions
class HasSetChild s where
    setChild :: ToCUIControlIO a => s -> a -> IO ()

class HasAppendChild s where
    appendChild :: ToCUIControlIO a => s -> a -> IO ()

class HasOnClosing w where
    onClosing :: w -> IO () -> IO ()

-- |
-- Displays a control ('c_uiControlShow')
uiShow :: ToCUIControl a => a -> IO ()
uiShow = c_uiControlShow . toCUIControl

-- |
-- Hides a control ('c_uiControlHide')
uiHide :: ToCUIControl a => a -> IO ()
uiHide = c_uiControlHide . toCUIControl

-- |
-- Destroys a control ('c_uiControlDestroy')
uiDestroy :: ToCUIControl a => a -> IO ()
uiDestroy = c_uiControlDestroy . toCUIControl

-- |
-- Get a control's parent ('c_uiControlParent')
uiGetParent :: ToCUIControl a => a -> IO CUIControl
uiGetParent = c_uiControlParent . toCUIControl

-- |
-- Set a control's parent ('c_uiControlSetParent')
uiSetParent :: (ToCUIControl a, ToCUIControl b) => a -> b -> IO ()
uiSetParent control parent =
    c_uiControlSetParent (toCUIControl control) (toCUIControl parent)

-- |
-- Get if a control is on the top level ('c_uiControlTopLevel')
uiGetTopLevel :: ToCUIControl a => a -> IO Bool
uiGetTopLevel c = numToBool <$> c_uiControlToplevel (toCUIControl c)

-- |
-- Get if a control is visible ('c_uiControlVisible')
uiGetVisible :: ToCUIControl a => a -> IO Bool
uiGetVisible c = numToBool <$> c_uiControlVisible (toCUIControl c)

-- |
-- Get if a control is enabled ('c_uiControlEnabled')
uiGetEnabled :: ToCUIControl a => a -> IO Bool
uiGetEnabled c = numToBool <$> c_uiControlEnabled (toCUIControl c)

-- |
-- Set if a control is enabled ('c_uiControlEnable' & 'c_uiControlDisable')
uiSetEnabled :: ToCUIControl a => a -> Bool -> IO ()
uiSetEnabled c True = c_uiControlEnable (toCUIControl c)
uiSetEnabled c False = c_uiControlDisable (toCUIControl c)

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
    c_wrap1I :: (DataPtr -> IO CInt) -> IO (FunPtr (DataPtr -> IO CInt))

-- |
-- Wrap a 1 argument event listener on a foreign pointer
foreign import ccall "wrapper"
    c_wrap1 :: (DataPtr -> IO ()) -> IO (FunPtr (DataPtr -> IO ()))

-- |
-- Wrap a 2 argument event listener on a foreign pointer
foreign import ccall "wrapper"
    c_wrap2 :: (DataPtr -> DataPtr -> IO ()) -> IO (FunPtr (DataPtr -> DataPtr -> IO ()))

-- * UI Controls

-- ** Windows
-- *** CUIWindow <- uiWindow

-- |
-- A C window
--
-- @
-- -- | An action that creates a window with a child
-- myWindow :: 'CUIControl' -> IO 'CUIWindow'
-- myButton cui = do
--     let title = "Hello World"
--         width = 680
--         height = 400
--         hasMenubar = True
--     win <- 'uiNewWindow' title width height hasMenubar
--     -- Get hold of the window pointer
--     win ``setChild`` cui
--     -- Add the control as a child of the window
--     return win
--     -- Return the pointer for later use
-- @
newtype CUIWindow = CUIWindow (Ptr RawWindow)
  deriving(Show, ToCUIControl)

data RawWindow

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
uiNewWindow t w h hasMenubar = withCString t $ \t' ->
    c_uiNewWindow t' (fromIntegral w) (fromIntegral h) (boolToNum hasMenubar)

uiWindowGetTitle :: CUIWindow -> IO String
uiWindowGetTitle = c_uiWindowTitle >=> peekCString

uiWindowGetPosition :: CUIWindow -> IO (Int, Int)
uiWindowGetPosition w = alloca $ \x -> alloca $ \y -> do
    c_uiWindowPosition w x y
    x' <- peek x
    y' <- peek y
    return (fromIntegral x', fromIntegral y')

uiWindowSetPosition :: CUIWindow -> (Int, Int) -> IO ()
uiWindowSetPosition w (x, y) =
    c_uiWindowSetPosition w (fromIntegral x) (fromIntegral y)

uiWindowCenter :: CUIWindow -> IO ()
uiWindowCenter = c_uiWindowCenter

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

-- | Get the window position
foreign import capi "ui.h uiWindowPosition"
    c_uiWindowPosition
        :: CUIWindow
        -> Ptr CInt
        -- ^ Pointer to x coordinate
        -> Ptr CInt
        -- ^ Pointer to y coordinate
        -> IO ()

-- | Set the window position
foreign import capi "ui.h uiWindowSetPosition"
    c_uiWindowSetPosition
        :: CUIWindow
        -> CInt
        -- ^ The x coordinate
        -> CInt
        -- ^ The y coordinate
        -> IO ()

-- | Center the window
foreign import capi "ui.h uiWindowCenter"
    c_uiWindowCenter :: CUIWindow -> IO ()

-- | Add a callback to the window's position
foreign import capi "ui.h uiWindowOnPositionChanged"
    c_uiWindowOnPositionChanged :: CUIWindow -> FunPtr (CUIWindow -> DataPtr -> IO ()) -> DataPtr -> IO ()

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

instance HasOnClosing CUIWindow where
    onClosing w a = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> a)
        c_uiWindowOnClosing w f nullPtr

-- | Is the window borderless?
foreign import capi "ui.h uiWindowBorderless"
    c_uiWindowBorderless :: CUIWindow -> IO CInt

-- | Set the window as borderless
foreign import capi "ui.h uiWindowSetBorderless"
    c_uiWindowSetBorderless :: CUIWindow -> CInt -> IO ()

-- | Set a child on the window
foreign import capi "ui.h uiWindowSetChild"
    c_uiWindowSetChild :: CUIWindow -> CUIControl -> IO ()

instance HasSetChild CUIWindow where
    setChild w c = do
        c' <- toCUIControlIO c
        c_uiWindowSetChild w c'

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
newtype CUILabel = CUILabel (Ptr RawLabel)
  deriving(Show, ToCUIControl)
data RawLabel

foreign import capi "ui.h uiLabelText"
    c_uiLabelText :: CUILabel -> IO CString

foreign import capi "ui.h uiLabelSetText"
    c_uiLabelSetText :: CUILabel -> CString -> IO ()

instance HasSetText CUILabel where
    setText c s = withCString s (c_uiLabelSetText c)

foreign import capi "ui.h uiNewLabel"
    c_uiNewLabel :: CString -> IO CUILabel

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

instance HasAppendChild CUIBox where
    appendChild b c = do
        c' <- toCUIControlIO c
        c_uiBoxAppend b c' 1

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

-- *** CUITab <- uiTab
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

-- *** CUIGroup <- uiGroup
newtype CUIGroup = CUIGroup (Ptr RawGroup)
  deriving(Show, ToCUIControl)
data RawGroup

foreign import capi "ui.h uiGroupTitle"
    c_uiGroupTitle :: CUIGroup -> IO CString

foreign import capi "ui.h uiGroupSetTitle"
    c_uiGroupSetTitle :: CUIGroup -> CString -> IO ()

foreign import capi "ui.h uiGroupSetChild"
    c_uiGroupSetChild :: CUIGroup -> CUIControl -> IO ()

instance HasSetChild CUIGroup where
    setChild g c = do
        c' <- toCUIControlIO c
        c_uiGroupSetChild g c'

foreign import capi "ui.h uiGroupMargined"
    c_uiGroupMargined :: CUIGroup -> IO CInt

foreign import capi "ui.h uiGroupSetMargined"
    c_uiGroupSetMargined :: CUIGroup -> CInt -> IO ()

foreign import capi "ui.h uiNewGroup"
    c_uiNewGroup :: CString -> IO CUIGroup

-- *** CUISeparator <- uiSeparator
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
-- A C button
--
-- @
-- -- | An action that creates a button
-- myButton :: IO 'CUIButton'
-- myButton = do
--     btn <- 'c_uiNewButton' =<< 'newCString' "Hello world"
--     -- Get hold of the button pointer
--     btn ``onClick`` print "Clicked!"
--     -- Add a 'onClick' handler to the control
--     return btn
--     -- Return the pointer for later use
-- @
newtype CUIButton = CUIButton (Ptr RawButton)
  deriving(Show, ToCUIControl)
data RawButton

foreign import capi "ui.h uiButtonOnClicked"
    c_uiButtonOnClicked :: CUIButton -> FunPtr (CUIButton -> DataPtr -> IO ()) -> DataPtr -> IO ()

instance HasOnClicked CUIButton where
    onClick btn action = do
        f <- castFunPtr <$> c_wrap2 (\_ _ -> action)
        c_uiButtonOnClicked btn f nullPtr

foreign import capi "ui.h uiButtonSetText"
    c_uiButtonSetText :: CUIButton -> CString -> IO ()

instance HasSetText CUIButton where
    setText btn s = withCString s (c_uiButtonSetText btn)

foreign import capi "ui.h uiButtonText"
    c_uiButtonText :: CUIButton -> CString

foreign import capi "ui.h uiNewButton"
    c_uiNewButton :: CString -> IO CUIButton

-- *** CUICheckbox <- uiCheckbox
newtype CUICheckbox = CUICheckbox (Ptr RawCheckbox)
  deriving(Show, ToCUIControl)
data RawCheckbox

foreign import capi "ui.h uiCheckboxText"
    c_uiCheckboxText :: CUICheckbox -> IO CString

foreign import capi "ui.h uiCheckboxSetText"
    c_uiCheckboxSetText :: CUICheckbox -> CString -> IO ()

instance HasSetText CUICheckbox where
    setText btn s = withCString s (c_uiCheckboxSetText btn)

foreign import capi "ui.h uiCheckboxOnToggled"
    c_uiCheckboxOnToggled :: CUICheckbox -> FunPtr (CUICheckbox -> DataPtr -> IO ()) -> DataPtr -> IO ()

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
    setText c s = withCString s (c_uiEntrySetText c)

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
newtype CUISlider = CUISlider (Ptr RawSlider)
  deriving(Show, ToCUIControl)
data RawSlider

foreign import capi "ui.h uiSliderValue"
    c_uiSliderValue :: CUISlider -> IO CInt

foreign import capi "ui.h uiSliderSetValue"
    c_uiSliderSetValue :: CUISlider -> CInt -> IO ()

instance HasSetValue CUISlider where
    setValue c i = c_uiSliderSetValue c (fromIntegral i)

foreign import capi "ui.h uiSliderOnChanged"
    c_uiSliderOnChanged :: CUISlider -> FunPtr (CUISlider -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewSlider"
    c_uiNewSlider :: CInt -> CInt -> IO CUISlider

-- *** CUICombobox <- uiCombobox
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
    c_uiComboboxOnSelected :: CUICombobox -> FunPtr (CUICombobox -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewCombobox"
    c_uiNewCombobox :: IO CUICombobox

-- *** CUIEditableCombobox <- uiEditableCombobox
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
    setText c s = withCString s (c_uiEditableComboboxSetText c)

foreign import capi "ui.h uiEditableComboboxOnChanged"
    c_uiEditableComboboxOnChanged :: CUIEditableCombobox -> FunPtr (CUIEditableCombobox -> DataPtr -> IO ()) -> DataPtr -> IO ()

foreign import capi "ui.h uiNewEditableCombobox"
    c_uiNewEditableCombobox :: IO CUIEditableCombobox

-- *** CUIRadioButtons <- uiRadioButtons
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

-- *** CUIMultilineEntry <- uiMultilineEntry
newtype CUIMultilineEntry = CUIMultilineEntry (Ptr RawMultilineEntry)
  deriving(Show, ToCUIControl)
data RawMultilineEntry

foreign import capi "ui.h uiMultilineEntryText"
    c_uiMultilineEntryText :: CUIMultilineEntry -> IO CString

foreign import capi "ui.h uiMultilineEntrySetText"
    c_uiMultilineEntrySetText :: CUIMultilineEntry -> CString -> IO ()

instance HasSetText CUIMultilineEntry where
    setText c s = withCString s (c_uiMultilineEntrySetText c)

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

instance HasGetValue CUIProgressBar where
    getValue c = fromIntegral <$> c_uiProgressBarValue c

foreign import ccall safe "uiProgressBarSetValue"
    c_uiProgressBarSetValue :: CUIProgressBar -> CInt -> IO ()

instance HasSetValue CUIProgressBar where
    setValue c i = c_uiProgressBarSetValue c (fromIntegral i)

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

instance HasSetValue CUISpinbox where
    setValue c i = c_uiSpinboxSetValue c (fromIntegral i)

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

uiNewMenu s = newCString s >>= c_uiNewMenu

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

-- * UI Alerts and Dialogs
foreign import capi "ui.h uiOpenFile"
    c_uiOpenFile :: CUIWindow -> IO CString

foreign import capi "ui.h uiSaveFile"
    c_uiSaveFile :: CUIWindow -> IO CString

foreign import capi "ui.h uiMsgBox"
    c_uiMsgBox :: CUIWindow -> CString -> CString -> IO ()

foreign import capi "ui.h uiMsgBoxError"
    c_uiMsgBoxError :: CUIWindow -> CString -> CString -> IO ()
