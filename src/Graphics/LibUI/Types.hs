{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.LibUI.Types
  where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Free.TH
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Default
import           Data.Maybe
import           Data.String
import           Foreign                  hiding (void)
import qualified Foreign
import           Foreign.C
-- import Data.Data

import           Graphics.LibUI.FFI
import           Graphics.LibUI.MonadUI

headMaybe [] = Nothing
headMaybe (c:_) = Just c

data UIControl c next = UIControlWindow (UIWindow c) next
                      | UIControlButton UIButton next
                      | UIControlBox (UIBox c) next
                      | UIControlCheckbox UICheckbox next
                      | UIControlEntry UIEntry next
                      | UIControlLabel UILabel next
                      | UIControlTab (UITab c) next
                      | UIControlGroup (UIGroup c) next
                      | UIControlSpinbox UISpinbox next
                      | UIControlSlider UISlider next
                      | UIControlProgressBar UIProgressBar next
                      | UIControlSeparator UISeparator next
                      | UIControlCombobox UICombobox next
                      | UIControlEditableCombobox UIEditableCombobox next
                      | UIControlRadioButtons UIRadioButtons next
                      | UIControlMultlineEntry UIMultilineEntry next
                      | UIControlMenuItem UIMenuItem next
                      | UIControlMenu UIMenu next
  deriving(Functor)

-- class ToCUIControlIO c where
--     toCUIControlIO :: c -> IO CUIControl

--instance {-# OVERLAPS #-} ToCUIControlIO' (UIControl CUIControl) CUIControl where
--    toCUIIO ctrl = do
--        ctrl' <- toCUIControl <$> case ctrl of
--            UIControlWindow c -> toCUIIO c :: IO CUIWindow
--            UIControlButton c -> toCUIIO c :: IO CUIButton
--            UIControlBox c -> toCUIIO c :: IO CUIBox
--            UIControlCheckbox c -> toCUIIO c :: IO CUICheckbox
--            UIControlEntry c -> toCUIIO c :: IO CUIEntry
--            UIControlLabel c -> toCUIIO c :: IO CUILabel
--            UIControlTab c -> toCUIIO c :: IO CUITabs
--            UIControlGroup c -> toCUIIO c :: IO CUIGroup
--            UIControlSpinbox c -> toCUIIO c :: IO CUISpinbox
--            UIControlSlider c -> toCUIIO c :: IO CUISlider
--            UIControlProgressBar c -> toCUIIO c :: IO CUIProgressBar
--            UIControlSeparator c -> toCUIIO c :: IO CUISeparator
--            UIControlCombobox c -> toCUIIO c :: IO CUICombobox
--            UIControlEditableCombobox c -> toCUIIO c :: IO CUIEditableCombobox
--            UIControlRadioButtons c -> toCUIIO c :: IO CUIRadioButtons
--            UIControlMultlineEntry c -> toCUIIO c :: IO CUIMultilineEntry
--            UIControlMenuItem c -> toCUIIO c :: IO CUIMenuItem
--            UIControlMenu c -> toCUIIO c :: IO CUIMenu
--        toCUIIO ctrl'

--data UI' c = UI' [UIControl c]
--class Monad m => MonadUI m r c where
--    runMonadUI :: UI' c -> m r

--instance MonadUI IO CUIControl CUIControl where
--    runMonadUI :: UI' CUIControl -> IO CUIControl
--    runMonadUI (UI' cs) = do
--        cs' <- mapM toCUIIO cs
--        return (head cs')

runUILoop ui = run
  where
    run = do
        uiInit
        (_, cs) <- runUI ui
        mapM_ uiShow cs
        uiOnShouldQuit (uiQuit >> return 0)
        uiMain

window :: String -> Int -> Int -> Bool -> UI () -> UI CUIWindow
window title width height hasMenubar child = UI $ do
    c <- toCUIIO $ def { uiWindowTitle = title
                       , uiWindowWidth = width
                       , uiWindowHeight = height
                       , uiWindowHasMenubar = hasMenubar
                       , uiWindowChild = child
                       }
    return (c, [toCUIControl c])

window' :: UIWindow (UI a) -> UI (CUIWindow, a)
window' w = UI $ do
    (x, [c]) <- runUI (uiWindowChild w)
    cw <- toCUIIO (w { uiWindowChild = c
                     })
    return ((cw, x), [toCUIControl cw])

-- window' :: UIWindow -> UI CUIWindow
-- window' win = UI $ do
--     cuiWin@(CUIControl cwin) <- toCUIControlIO win
--     return (CUIWindow (castPtr cwin), [cuiWin])

-- button :: String -> UI ()
-- button title = wrap (UIButton title Nothing)

-- wrap :: ToCUIControlIO c => c -> UI b
wrap toCUI = UI $ do
    cui@(CUIControl ptr) <- toCUIControlIO toCUI
    -- let cuip = toCUIPointerType ptr
    return ((), [cui])

wrapEmpty :: ToCUIControlIO c => c -> UI ()
wrapEmpty toCUI = UI $ do
    cui <- toCUIControlIO toCUI
    return ((), [])

vbox :: UI a -> UI a
vbox = box UIVerticalBox

hbox :: UI a -> UI a
hbox = box UIHorizontalBox

box :: (Bool -> [UIBoxChild CUIControl] -> UIBox CUIControl) -> UI a -> UI a
box boxtype ui = UI $ do
    (x, cs) <- runUI ui -- :: IO (a, [CUIControl])
    c <- toCUIIO $ boxtype True (map (UIBoxChild False) cs) :: IO CUIBox
    return (x, [toCUIControl c])


menu :: String -> [UIMenuItem] -> UI CUIMenu
menu name items = UI $ do
    c <- toCUIIO $ UIMenu name items
    return (c, [])

group :: String -> UI a -> UI (CUIGroup, a)
group title items = UI $ do
    (x, child) <- runUI items
    child' <- toCUIIO child :: IO CUIBox
    c <- toCUIIO $ UIGroup title 1 child' :: IO CUIGroup
    return ((c, x), [toCUIControl c])

progressbar :: Int -> UI CUIProgressBar
progressbar value = UI $ do
    pg <- toCUIIO (UIProgressBar value)
    return (pg, [toCUIControl pg])

slider :: Int -> Int -> Int -> UI CUISlider
slider value min max = UI $ do
    sld <- toCUIIO (UISlider value min max)
    return (sld, [toCUIControl sld])

spinbox :: Int -> Int -> Int -> UI CUISpinbox
spinbox value min max = UI $ do
    spb <- toCUIIO (UISpinbox value min max)
    return (spb, [toCUIControl spb])

-- render = wrap

-- tabs ts = UI $ do
--     ts' <- forM ts $ \t -> do
--         (r, (c:cs)) <- runUI t
--         return (r, c)
--     c <- toCUIControlIO (UITab 1 ts')
--     return ((), [c])
tabs :: Writer [UI String] () -> UI CUITabs
tabs wts = UI $ do
    let ts :: [UI String]
        ts = snd $ runWriter wts
    ts' <- forM ts $ \t -> do
        (r, c:_) <- runUI t
        return (r, c)
    t <- toCUIIO (UITab 1 ts') :: IO CUITabs
    return (t, [toCUIControl t])

tab :: String -> UI () -> Writer [UI String] ()
tab title ui = do
    let ui' = UI $ do
            (_, c) <- runUI $ vbox ui
            return (title, c)
    tell [ui']

checkbox :: String -> UI CUICheckbox
checkbox t = UI $ do
    c <- toCUIIO (UICheckbox False t)
    return (c, [toCUIControl c])

button :: UIButton -> UI CUIButton
button UIButton{..} = UI $ do
    cbtn <- c_uiNewButton =<< newCString uiButtonText
    maybe (return ())
          (\onClick -> do
               cb <- c_wrap2 (\_ _ -> onClick)
               c_uiButtonOnClicked cbtn (castFunPtr cb) nullPtr)
          uiButtonOnClicked
    return (cbtn, [toCUIControl cbtn])

label :: String -> UI CUILabel
label t = UI $ do
    lbl <- toCUIIO (UILabel t)
    return (lbl, [toCUIControl lbl])

entry :: String -> UI CUIEntry
entry t = UI $ do
    c <- toCUIIO (UIEntry False t)
    return (c, [toCUIControl c])

searchEntry :: String -> UI CUIEntry
searchEntry t = UI $ do
    c <- toCUIIO (UISearchEntry False t)
    return (c, [toCUIControl c])

passwordEntry :: String -> UI CUIEntry
passwordEntry t = UI $ do
    c <- toCUIIO (UIPasswordEntry False t)
    return (c, [toCUIControl c])

form :: [(String, UI a)] -> UI CUIForm
form cs = UI $ do
    c <- toCUIIO (UIForm cs)
    return (c, [toCUIControl c])
formItem x e = (x, e)

-- ** Windows
instance {-# OVERLAPS #-} ToCUIControlIO' [CUIControl] CUIBox where
    toCUIIO cs = do
        vb <- c_uiNewVerticalBox
        forM_ cs $ \c -> do
            c_uiBoxAppend vb c 1
        return vb

instance {-# OVERLAPS #-} ToCUIControlIO' (UI a) CUIBox where
    toCUIIO :: UI a -> IO CUIBox
    toCUIIO ui = do
        (_, cs) <- runUI ui :: IO (a, [CUIControl])
        cs' <- toCUIIO cs :: IO CUIBox
        return cs'

data UIWindow c =
    UIWindow { uiWindowTitle                :: String
             , uiWindowWidth                :: Int
             , uiWindowHeight               :: Int
             , uiWindowHasMenubar           :: Bool
             , uiWindowMargined             :: Bool
             , uiWindowChild                :: c
             , uiWindowOnContentSizeChanged :: Maybe ((Int, Int) -> IO ())
             , uiWindowDidMount             :: Maybe (IO ())
             , uiWindowOnClosing            :: Maybe (IO ())
             }

instance Default (UIWindow c) where
    def = UIWindow { uiWindowTitle = "haskell-libui"
                   , uiWindowWidth = 680
                   , uiWindowHeight = 300
                   , uiWindowHasMenubar = True
                   , uiWindowMargined = True
                   , uiWindowOnClosing = Just uiQuit
                   , uiWindowOnContentSizeChanged = Nothing
                   , uiWindowDidMount = Nothing
                   , uiWindowChild = error "uiWindowChild needs to be overwritten"
                   }

instance {-# OVERLAPPING #-} ToCUIControlIO' (UIWindow (UI ())) CUIWindow where
    toCUIIO :: UIWindow (UI ()) -> IO CUIWindow
    toCUIIO wnd@UIWindow{..} = do
        box <- toCUIIO uiWindowChild :: IO CUIBox
        toCUIIO wnd { uiWindowChild = toCUIControl box
                    }

instance {-# OVERLAPPING #-} ToCUIControlIO' (UIWindow CUIControl) CUIWindow where
    toCUIIO :: UIWindow CUIControl -> IO CUIWindow
    toCUIIO wnd@UIWindow{..} = do
        w <- uiNewWindow uiWindowTitle uiWindowWidth uiWindowHeight uiWindowHasMenubar
        maybe (return ()) (onClosing w) uiWindowOnClosing
        w `setMargined` uiWindowMargined
        w `setChild` uiWindowChild
        return w

-- ** Buttons
data UIButton = UIButton { uiButtonText      :: String
                         , uiButtonOnClicked :: Maybe (IO ())
                         }

instance {-# OVERLAPPING #-} Default UIButton  where
    def = UIButton { uiButtonText = "Button"
                   , uiButtonOnClicked = Nothing
                   }

instance {-# OVERLAPPING #-} ToCUIControlIO' UIButton CUIButton where
    toCUIIO UIButton{..} = do
        cbtn <- c_uiNewButton =<< newCString uiButtonText
        maybe
            (return ())
            (\onClick -> do
                    cb <- c_wrap2 (\_ _ -> onClick)
                    c_uiButtonOnClicked cbtn (castFunPtr cb) nullPtr)
            uiButtonOnClicked
        return cbtn

-- ** Boxes
data UIBox c = UIHorizontalBox { uiBoxPadded   :: Bool
                               , uiBoxChildren :: [UIBoxChild c]
                               }
             | UIVerticalBox { uiBoxPadded   :: Bool
                             , uiBoxChildren :: [UIBoxChild c]
                             }

instance Default (UIBox c) where
    def = UIVerticalBox { uiBoxPadded = True
                        , uiBoxChildren = []
                        }

data UIBoxChild c = UIBoxChild { uiBoxChildStretchy :: Bool
                               , uiBoxChildControl  :: c
                               }

instance {-# OVERLAPS #-} ToCUIControlIO' c CUIControl => ToCUIControlIO' (UIBox c) CUIBox where
    toCUIIO ui = do
        b <- case ui of
            UIVerticalBox{} -> c_uiNewVerticalBox
            UIHorizontalBox{} -> c_uiNewHorizontalBox
        b `setPadded` uiBoxPadded ui
        forM_ (uiBoxChildren ui) $ \UIBoxChild{..} -> do
            uiBoxChildControl' <- toCUIIO uiBoxChildControl :: IO CUIControl
            let uiBoxChildStretchy' = if uiBoxChildStretchy then 1 else 0
            c_uiBoxAppend b uiBoxChildControl' uiBoxChildStretchy'
        return b

-- ** Checkboxes
data UICheckbox = UICheckbox { uiCheckboxChecked :: Bool
                             , uiCheckboxText    :: String
                             }

instance {-# OVERLAPS #-} ToCUIControlIO' UICheckbox CUICheckbox where
    toCUIIO UICheckbox{..} = do
        c <- c_uiNewCheckbox =<< newCString uiCheckboxText
        c_uiCheckboxSetChecked c (if uiCheckboxChecked then 1 else 0)
        return c

-- ** Text inputs
data UIEntry = UIEntry { uiEntryReadOnly :: Bool
                       , uiEntryText     :: String
                       }
             | UIPasswordEntry { uiEntryReadOnly :: Bool
                               , uiEntryText     :: String
                               }
             | UISearchEntry { uiEntryReadOnly :: Bool
                             , uiEntryText     :: String
                             }

mkEntry mk entry = do
    e <- mk
    c_uiEntrySetText e =<< newCString (uiEntryText entry)
    c_uiEntrySetReadOnly e (if uiEntryReadOnly entry then 1 else 0)
    return e

instance {-# OVERLAPS #-} ToCUIControlIO' UIEntry CUIEntry where
    toCUIIO e@UIEntry{} = mkEntry c_uiNewEntry e
    toCUIIO e@UIPasswordEntry{} = mkEntry c_uiNewPasswordEntry e
    toCUIIO e@UISearchEntry{} = mkEntry c_uiNewSearchEntry e

-- ** Labels
data UILabel = UILabel { uiLabelText :: String
                       }

instance Default UILabel where
    def = UILabel { uiLabelText = ""
                  }

instance {-# OVERLAPS #-} ToCUIControlIO' UILabel CUILabel where
    toCUIIO UILabel{..} =
        c_uiNewLabel =<< newCString uiLabelText

-- ** Text Forms
data UIForm = forall c. UIForm [(String, UI c)]

instance {-# OVERLAPS #-} ToCUIControlIO' UIForm CUIForm where
    toCUIIO (UIForm cs) = do
        f <- c_uiNewForm
        c_uiFormSetPadded f 10
        forM_ cs $ \(n, c) -> do
            n' <- newCString n
            (_, [c']) <- runUI c
            c_uiFormAppend f n' c' 1
        return f

-- ** Tabs
data UITab c =
    UITab { uiTabMargin   :: Int
          , uiTabChildren :: [(String, c)]
          }

instance {-# OVERLAPS #-} ToCUIControlIO' c CUIControl => ToCUIControlIO' (UITab c) CUITabs where
    toCUIIO UITab{..} = do
        t <- c_uiNewTab
        forM_ uiTabChildren $ \(n, c) -> do
            print n
            n' <- newCString n
            c' <- toCUIIO c :: IO CUIControl
            c_uiTabAppend t n' c'
        -- c_uiTabSetMargined t (fromIntegral uiTabMargin) 0
        return t

-- ** Groups
data UIGroup c =
    UIGroup { uiGroupTitle  :: String
            , uiGroupMargin :: Int
            , uiGroupChild  :: c
            }

instance {-# OVERLAPS #-} ToCUIControlIO' (UI ()) CUIBox where
    toCUIIO :: UI () -> IO CUIBox
    toCUIIO ui = do
        (_, c) <- runUI ui
        toCUIIO c

instance {-# OVERLAPS #-} ToCUIControlIO' (UI ()) CUIControl where
    toCUIIO :: UI () -> IO CUIControl
    toCUIIO ui = do
        cbox <- toCUIIO ui :: IO CUIBox
        return (toCUIControl cbox)

instance {-# OVERLAPS #-} ToCUIControlIO' c CUIControl => ToCUIControlIO' (UIGroup c) CUIGroup where
    toCUIIO UIGroup{..} = do
        g <- c_uiNewGroup =<< newCString uiGroupTitle
        c_uiGroupSetMargined g (fromIntegral uiGroupMargin)
        child <- toCUIIO uiGroupChild :: IO CUIControl
        c_uiGroupSetChild g child
        return g

-- ** Sliders
data UISpinbox = UISpinbox { uiSpinboxValue :: Int
                           , uiSpinboxMin   :: Int
                           , uiSpinboxMax   :: Int
                           }

instance {-# OVERLAPS #-} ToCUIControlIO' UISpinbox CUISpinbox where
    toCUIIO UISpinbox{..} = do
        sb <- c_uiNewSpinbox (fromIntegral uiSpinboxMin) (fromIntegral uiSpinboxMax)
        c_uiSpinboxSetValue sb (fromIntegral uiSpinboxValue)
        -- c_uiProgressBarSetValue pb (fromIntegral uiProgressBarValue)
        return sb

data UISlider = UISlider { uiSliderValue :: Int
                         , uiSliderMin   :: Int
                         , uiSliderMax   :: Int
                         }

instance {-# OVERLAPS #-} ToCUIControlIO' UISlider CUISlider where
    toCUIIO UISlider{..} = do
        s <- c_uiNewSlider (fromIntegral uiSliderMin) (fromIntegral uiSliderMax)
        c_uiSliderSetValue s (fromIntegral uiSliderValue)
        return s

data UIProgressBar = UIProgressBar { uiProgressBarValue :: Int
                                   }

instance ToCUIControlIO' UIProgressBar CUIProgressBar where
    toCUIIO UIProgressBar{..} = do
        pb <- c_uiNewProgressBar
        c_uiProgressBarSetValue pb (fromIntegral uiProgressBarValue)
        return pb

-- ** Separators
data UISeparator = UIHorizontalSeparator
                 | UIVerticalSeparator

instance {-# OVERLAPS #-} ToCUIControlIO' UISeparator CUISeparator where
    toCUIIO UIHorizontalSeparator = c_uiNewHorizontalSeparator
    toCUIIO UIVerticalSeparator = c_uiNewVerticalSeparator

-- ** Selects
data UICombobox = UICombobox { uiComboboxSelected :: Bool
                             }

instance {-# OVERLAPS #-} ToCUIControlIO' UICombobox CUICombobox where
    toCUIIO UICombobox{..} = do
        cb <- c_uiNewCombobox
        c_uiComboboxSetSelected cb (if uiComboboxSelected then 1 else 0)
        return cb

data UIEditableCombobox = UIEditableCombobox { uiEditableComboboxText :: String
                                             }

instance {-# OVERLAPS #-} ToCUIControlIO' UIEditableCombobox CUIEditableCombobox where
    toCUIIO UIEditableCombobox{..} = do
        cb <- c_uiNewEditableCombobox
        c_uiEditableComboboxSetText cb =<< newCString uiEditableComboboxText
        return cb

data UIRadioButtons = UIRadioButtons { uiRadioButtonsSelected :: Int
                                     }

instance {-# OVERLAPS #-} ToCUIControlIO' UIRadioButtons CUIRadioButtons where
    toCUIIO UIRadioButtons{..} =
        c_uiNewRadioButtons

-- ** Textarea
data UIMultilineEntry = UIMultilineEntry { uiMultilineEntryText     :: String
                                         , uiMultilineEntryReadOnly :: Bool
                                         }

instance {-# OVERLAPS #-} ToCUIControlIO' UIMultilineEntry CUIMultilineEntry where
    toCUIIO UIMultilineEntry{..} =
        c_uiNewMultilineEntry

-- ** Menus

-- |
-- The application menu. Either a window menu, as in Windows/Linux or the top
-- bar menu in OSX.
--
-- Renders with `uiMenu` 'c_uiNewMenu', using 'CUIMenu'
data UIMenu = UIMenu { uiMenuName  :: String
                     , uiMenuItems :: [UIMenuItem]
                     }

instance {-# OVERLAPS #-} ToCUIControlIO' UIMenu CUIMenu where
    toCUIIO UIMenu{..} = do
        m <- c_uiNewMenu =<< newCString uiMenuName
        forM_ uiMenuItems $ \item ->
            appendMenuItem m item
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

