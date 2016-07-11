{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
module Graphics.LibUI
  where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.String
import           Foreign                  hiding (void)
import qualified Foreign                  as Foreign
import           Foreign.C

-- data UIControl = UIControlWindow UIWindow
--                | UIControlButton UIButton
--                | UIControlBox UIBox
--                | UIControlCheckbox UICheckbox
--                | UIControlEntry UIEntry
--                | UIControlLabel UILabel
--                | UIControlTab UITab
--                | UIControlGroup UIGroup
--                | UIControlSpinbox UISpinbox
--                | UIControlSlider UISlider
--                | UIControlProgressBar UIProgressBar
--                | UIControlSeparator UISeparator
--                | UIControlCombobox UICombobox
--                | UIControlEditableCombobox UIEditableCombobox
--                | UIControlRadioButtons UIRadioButtons
--                | UIControlMultlineEntry UIMultlineEntry
--                | UIControlMenuItem UIMenuItem
--                | UIControlMenu UIMenu

data Term a = Term { runTerm :: IO CUIControl
                   }

term :: ToCUIControl t => t -> Term t
term t = Term (toCUIControl t)

data UI a = UI { runUI :: IO (a, [CUIControl])
               }

instance Functor UI where
    f `fmap` ui = UI $ runUI ui >>= \(a, c) -> return (f a, c)

instance Monoid a => Monoid (UI a) where
    mempty = UI (return (mempty, []))
    ui1 `mappend` ui2 = UI $ do
        (a, cui1) <- runUI ui1
        (b, cui2) <- runUI ui2
        return (a `mappend` b, cui1 ++ cui2)

instance Applicative UI where
    pure = return
    (<*>) = ap

instance Monad UI where
    return x = UI (return (x, []))
    ui >>= a = UI $ do
        (r, cui1) <- runUI ui
        (r', cui2) <- runUI $ a r
        return (r', cui1 ++ cui2)

wrap toCUI = UI $ do
    cui <- toCUIControl toCUI
    return ((), [cui])

runUILoop :: UI a -> IO ()
runUILoop ui = run >> putStrLn "I'm still here"
  where
    run = do
        alloca $ \ptr -> do
            poke ptr (CSize (fromIntegral (sizeOf (CSize 0))))
            c_uiInit ptr
        (_, cs) <- runUI ui
        mapM_ c_uiControlShow cs
        forM_ cs $ \c -> do
            cb <- c_wrap2 (const (const (void abort)))
            c_uiWindowOnClosing c cb nullPtr
        cb <- c_wrap1I (const abort)
        c_uiOnShouldQuit cb nullPtr
        a <- async $ c_uiMain
        wait a `catch` \UserInterrupt -> print "interrupt" >> void abort
    abort = do
        c_uiUninit
        return 0

window title width height hasMenubar child = UI $ do
    c <- toCUIControl $ UIWindow title width height hasMenubar child
    return ((), [c])

button title = wrap (UIButton title)

vbox ui = UI $ do
    (x, cs) <- runUI ui
    c <- toCUIControl $ UIVerticalBox 0 (map (False,) cs)
    return (x, [c])

menu name items = UI $ do
    c <- toCUIControl $ UIMenu name items
    c `seq` (return ())
    return ((), [])

stuff :: IO ()
stuff = runUILoop ui
  where
    ui :: UI ()
    ui = do
        menu "File" [ "Open"
                    , "Save"
                    , UIMenuItemQuit
                    ]
        window "Stuff" 640 300 True $ vbox $ do
            button "Click me 1"
            button "Click me 2"
        -- button "Click me"

class ToCUIControl c where
    toCUIControl :: c -> IO CUIControl

instance ToCUIControl UIWindow where
    toCUIControl UIWindow{..} = do
        w <- join $ c_uiNewWindow
            <$> newCString uiWindowTitle
            <*> return (fromIntegral uiWindowWidth)
            <*> return (fromIntegral uiWindowHeight)
            <*> return (if uiWindowHasMenubar then 1 else 0)
        (_, cs) <- runUI uiWindowChild
        case cs of
            (c:_) -> c_uiWindowSetChild w c
            _ -> return ()
        return w

instance ToCUIControl UIButton where
    toCUIControl UIButton{..} =
        join $ c_uiNewButton
            <$> newCString uiButtonText

instance ToCUIControl UIBox where
    toCUIControl UIHorizontalBox{..} = do
        b <- c_uiNewHorizontalBox
        c_uiBoxSetPadded b (fromIntegral uiBoxPadding)
        mapM_ (uncurry (c_uiBoxAppend b))
            (map (\(b, c) -> (c, if b then 1 else 0)) uiBoxChildren)
        return b
    toCUIControl UIVerticalBox{..} = do
        b <- c_uiNewVerticalBox
        c_uiBoxSetPadded b (fromIntegral uiBoxPadding)
        mapM_ (uncurry (c_uiBoxAppend b))
            (map (\(b, c) -> (c, if b then 1 else 0)) uiBoxChildren)
        return b

instance ToCUIControl UICheckbox where
    toCUIControl UICheckbox{..} = do
        c <- c_uiNewCheckbox =<< newCString uiCheckboxText
        c_uiCheckboxSetChecked c (if uiCheckboxChecked then 1 else 0)
        return c

instance ToCUIControl UIEntry where
    toCUIControl UIEntry{..} = do
        e <- c_uiNewEntry
        c_uiEntrySetText e =<< newCString uiEntryText
        c_uiEntrySetReadOnly e (if uiEntryReadOnly then 1 else 0)
        return e

instance ToCUIControl UILabel where
    toCUIControl UILabel{..} = do
        c_uiNewLabel =<< newCString uiLabelText

instance ToCUIControl UITab where
    toCUIControl UITab{..} = do
        t <- c_uiNewTab
        c_uiTabSetMargined t (fromIntegral uiTabMargin)
        mapM_ (uncurry (c_uiTabAppend t)) =<< (forM uiTabChildren $ \(n, c) -> do
            n' <- newCString n
            return (n', c))
        return t

instance ToCUIControl UIGroup where
    toCUIControl UIGroup{..} = do
        g <- c_uiNewGroup =<< newCString uiGroupTitle
        c_uiGroupSetMargined g (fromIntegral uiGroupMargin)
        c_uiGroupSetChild g uiGroupChild
        return g

instance ToCUIControl UIMenu where
    toCUIControl UIMenu{..} = do
        m <- c_uiNewMenu =<< newCString uiMenuName
        forM_ uiMenuItems $ \item ->
            appendMenuItem m item
        print "Menu created"
        return m

appendMenuItem m UIMenuItem{..} = do
    ctext <- newCString uiMenuItemText
    c_uiMenuAppendItem m ctext
appendMenuItem m UIMenuItemQuit = do
    c_uiMenuAppendQuitItem m

-- getCUIControl _ = undefined

data UIWindow = UIWindow { uiWindowTitle      :: String
                         , uiWindowWidth      :: Int
                         , uiWindowHeight     :: Int
                         , uiWindowHasMenubar :: Bool
                         , uiWindowChild      :: UI ()
                         }

data UIButton = UIButton { uiButtonText :: String
                         }

data UIBox = UIHorizontalBox { uiBoxPadding  :: Int
                             , uiBoxChildren :: [(Bool, CUIControl)]
                             }
           | UIVerticalBox { uiBoxPadding  :: Int
                           , uiBoxChildren :: [(Bool, CUIControl)]
                           }

data UICheckbox = UICheckbox { uiCheckboxChecked :: Bool
                             , uiCheckboxText    :: String
                             }

data UIEntry = UIEntry { uiEntryReadOnly :: Bool
                       , uiEntryText     :: String
                       }
             -- TODO - | UIPasswordEntry {
             --                   }

data UILabel = UILabel { uiLabelText :: String
                       }

data UITab = UITab { uiTabMargin   :: Int
                   , uiTabChildren :: [(String, CUIControl)]
                   }

data UIGroup = UIGroup { uiGroupTitle  :: String
                       , uiGroupMargin :: Int
                       , uiGroupChild  :: CUIControl
                       }

data UISpinbox = UISpinbox { uiSpinboxValue :: Int
                           , uiSpinboxMin   :: Int
                           , uiSpinboxMax   :: Int
                           }

data UISlider = UISlider { uiSliderValue :: Int
                         , uiSliderMin   :: Int
                         , uiSliderMax   :: Int
                         }

data UIProgressBar = UIProgressbar { uiProgressbarValue :: Int
                                   }

data UISeparator = UIHorizontalSeparator
                 | UIVerticalSeparator

data UICombobox = UICombobox { uiComboboxSelected :: Bool
                             }

data UIEditableCombobox = UIEditableCombobox { uiEditableComboboxText :: String
                                             }

data UIRadioButtons = UIRadioButtons { uiRadioButtonsSelected :: Int
                                     }

data UIMultlineEntry = UIMultilineEntry { uiMultilineEntryText     :: String
                                        , uiMultilineEntryReadOnly :: Bool
                                        }

data UIMenuItem = UIMenuItem { uiMenuItemEnabled :: Bool
                             , uiMenuItemChecked :: Bool
                             , uiMenuItemText    :: String
                             }
                | UIMenuItemQuit

instance IsString UIMenuItem where
    fromString s = UIMenuItem { uiMenuItemEnabled = True
                              , uiMenuItemChecked = True
                              , uiMenuItemText = s
                              }

data UIMenu = UIMenu { uiMenuName  :: String
                     , uiMenuItems :: [UIMenuItem]
                     }

type VoidPtr = Ptr ()
type CUIControl = VoidPtr

foreign import ccall safe "uiMain"
    c_uiMain :: IO ()

foreign import ccall "uiQuit"
    c_uiQuit :: IO ()

foreign import ccall "uiUninit"
    c_uiUninit :: IO ()

foreign import ccall "uiInit"
    c_uiInit :: Ptr CSize -> IO ()

foreign import ccall "uiOnShouldQuit"
    c_uiOnShouldQuit :: FunPtr (VoidPtr -> IO CInt) -> VoidPtr -> IO ()

foreign import ccall "uiControlShow"
    c_uiControlShow :: CUIControl -> IO ()

foreign import ccall "wrapper"
    c_wrap1I :: (VoidPtr -> IO CInt) -> IO (FunPtr (VoidPtr -> IO CInt))

foreign import ccall "wrapper"
    c_wrap1 :: (VoidPtr -> IO ()) -> IO (FunPtr (VoidPtr -> IO ()))

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
    c_uiNewCheckbox :: CString -> IO CUICheckbox

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
    c_uiNewMenu :: CString -> IO CUIMenu

-- UI alerts
foreign import ccall "uiOpenFile"
    c_uiOpenFile :: CUIWindow -> IO CString

foreign import ccall "uiSaveFile"
    c_uiSaveFile :: CUIWindow -> IO CString

foreign import ccall "uiMsgBox"
    c_uiMsgBox :: CUIWindow -> CString -> CString -> IO ()

foreign import ccall "uiMsgBoxError"
    c_uiMsgBoxError :: CUIWindow -> CString -> CString -> IO ()
