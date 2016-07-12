{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Graphics.LibUI.Types
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
import qualified Foreign
import           Foreign.C

import           Graphics.LibUI.FFI
import           Graphics.LibUI.MonadUI

-- |
-- Something that can be rendered with libui
class ToCUIControl c where
    toCUIControl :: c -> IO CUIControl

data UIControl = UIControlWindow UIWindow
               | UIControlButton UIButton
               | UIControlBox UIBox
               | UIControlCheckbox UICheckbox
               | UIControlEntry UIEntry
               | UIControlLabel UILabel
               | UIControlTab UITab
               | UIControlGroup UIGroup
               | UIControlSpinbox UISpinbox
               | UIControlSlider UISlider
               | UIControlProgressBar UIProgressBar
               | UIControlSeparator UISeparator
               | UIControlCombobox UICombobox
               | UIControlEditableCombobox UIEditableCombobox
               | UIControlRadioButtons UIRadioButtons
               | UIControlMultlineEntry UIMultlineEntry
               | UIControlMenuItem UIMenuItem
               | UIControlMenu UIMenu

wrap :: ToCUIControl c => c -> UI ()
wrap toCUI = UI $ do
    cui <- toCUIControl toCUI
    return ((), [cui])

wrapEmpty :: ToCUIControl c => c -> UI ()
wrapEmpty toCUI = UI $ do
    cui <- toCUIControl toCUI
    return ((), [])

runUILoop :: UI a -> IO ()
runUILoop ui = run >> putStrLn "I'm still here"
  where
    run = do
        alloca $ \ptr -> do
            poke ptr (CSize (fromIntegral (sizeOf (CSize 0))))
            c_uiInit ptr
        (_, cs) <- runUI ui
        mapM_ c_uiControlShow cs

        mvRunning <- newEmptyMVar

        forM_ cs $ \c -> do
            cb <- c_wrap2 (const (const (void (abort mvRunning))))
            c_uiWindowOnClosing c cb nullPtr
        cb <- c_wrap1I (const (abort mvRunning))
        c_uiOnShouldQuit cb nullPtr

        -- c_uiMainSteps
        print "Start"

        loop mvRunning
        takeMVar mvRunning `catch` \UserInterrupt -> do
            print "interrupt"
            void (abort mvRunning)
    loop mvRunning =
        c_uiMain
        -- e <- isEmptyMVar mvRunning
        -- if True -- || e
        --     then do
        --         c_uiMainStep 1
        --         loop mvRunning
        --     else return ()

    abort mvRunning = do
        print "abort"
        c_uiQuit
        tryPutMVar mvRunning ()
        return 0

window :: String -> Int -> Int -> Bool -> UI () -> UI ()
window title width height hasMenubar child = UI $ do
    c <- toCUIControl $ UIWindow title width height hasMenubar child
    return ((), [c])

button :: String -> UI ()
button title = wrap (UIButton title)

vbox :: UI a -> UI a
vbox ui = UI $ do
    (x, cs) <- runUI ui
    c <- toCUIControl $ UIVerticalBox 0 (map (False,) cs)
    return (x, [c])

menu :: String -> [UIMenuItem] -> UI ()
menu name items = UI $ do
    c <- toCUIControl $ UIMenu name items
    c `seq` return ()
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
            button "Click me 3"
        -- button "Click me"

-- ** Windows

data UIWindow = UIWindow { uiWindowTitle      :: String
                         , uiWindowWidth      :: Int
                         , uiWindowHeight     :: Int
                         , uiWindowHasMenubar :: Bool
                         , uiWindowChild      :: UI ()
                         }

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

-- ** Buttons

data UIButton = UIButton { uiButtonText :: String
                         }

instance ToCUIControl UIButton where
    toCUIControl UIButton{..} =
        join $ c_uiNewButton
            <$> newCString uiButtonText

data UIBox = UIHorizontalBox { uiBoxPadding  :: Int
                             , uiBoxChildren :: [(Bool, CUIControl)]
                             }
           | UIVerticalBox { uiBoxPadding  :: Int
                           , uiBoxChildren :: [(Bool, CUIControl)]
                           }

instance ToCUIControl UIBox where
    toCUIControl UIHorizontalBox{..} = do
        b <- c_uiNewHorizontalBox
        c_uiBoxSetPadded b (fromIntegral uiBoxPadding)
        forM_ uiBoxChildren $ \(bc, c) -> do
            let bc' = if bc then 1 else 0
            c_uiBoxAppend b c bc'
        return b
    toCUIControl UIVerticalBox{..} = do
        b <- c_uiNewVerticalBox
        c_uiBoxSetPadded b (fromIntegral uiBoxPadding)
        forM_ uiBoxChildren $ \(bc, c) -> do
            let bc' = if bc then 1 else 0
            c_uiBoxAppend b c bc'
        return b

-- |
-- Appends a menu item to a CUIBox
appendItem UIVerticalBox{..} b =
    forM_ uiBoxChildren $ \(bc, c) -> do
        let bc' = if bc then 1 else 0
        c_uiBoxAppend b c bc'

data UICheckbox = UICheckbox { uiCheckboxChecked :: Bool
                             , uiCheckboxText    :: String
                             }

instance ToCUIControl UICheckbox where
    toCUIControl UICheckbox{..} = do
        c <- c_uiNewCheckbox =<< newCString uiCheckboxText
        c_uiCheckboxSetChecked c (if uiCheckboxChecked then 1 else 0)
        return c

data UIEntry = UIEntry { uiEntryReadOnly :: Bool
                       , uiEntryText     :: String
                       }
             -- TODO - | UIPasswordEntry {
             --                   }

instance ToCUIControl UIEntry where
    toCUIControl UIEntry{..} = do
        e <- c_uiNewEntry
        c_uiEntrySetText e =<< newCString uiEntryText
        c_uiEntrySetReadOnly e (if uiEntryReadOnly then 1 else 0)
        return e

data UILabel = UILabel { uiLabelText :: String
                       }

instance ToCUIControl UILabel where
    toCUIControl UILabel{..} =
        c_uiNewLabel =<< newCString uiLabelText


data UITab = UITab { uiTabMargin   :: Int
                   , uiTabChildren :: [(String, CUIControl)]
                   }

instance ToCUIControl UITab where
    toCUIControl UITab{..} = do
        t <- c_uiNewTab
        c_uiTabSetMargined t (fromIntegral uiTabMargin) 0
        forM_ uiTabChildren $ \(n, c) -> do
            n' <- newCString n
            c_uiTabAppend t n' c
        return t

data UIGroup = UIGroup { uiGroupTitle  :: String
                       , uiGroupMargin :: Int
                       , uiGroupChild  :: CUIControl
                       }

instance ToCUIControl UIGroup where
    toCUIControl UIGroup{..} = do
        g <- c_uiNewGroup =<< newCString uiGroupTitle
        c_uiGroupSetMargined g (fromIntegral uiGroupMargin)
        c_uiGroupSetChild g uiGroupChild
        return g

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

-- ** Menus

-- |
-- The application menu. Either a window menu, as in Windows/Linux or the top
-- bar menu in OSX.
--
-- Renders with `uiMenu` 'c_uiNewMenu', using 'CUIMenu'
data UIMenu = UIMenu { uiMenuName  :: String
                     , uiMenuItems :: [UIMenuItem]
                     }


instance ToCUIControl UIMenu where
    toCUIControl UIMenu{..} = do
        m <- c_uiNewMenu =<< newCString uiMenuName
        forM_ uiMenuItems $ \item ->
            appendMenuItem m item
        print "Menu created"
        return m

-- |
-- Appends a menu item to a CUIMenu
appendMenuItem :: CUIMenu -> UIMenuItem -> IO CUIMenuItem
appendMenuItem m UIMenuItem{..} = do
    ctext <- newCString uiMenuItemText
    c_uiMenuAppendItem m ctext
appendMenuItem m UIMenuItemQuit =
    c_uiMenuAppendQuitItem m

-- |
-- Menu items
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

