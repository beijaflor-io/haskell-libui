{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Default
import           Data.String
import           Foreign                  hiding (void)
import qualified Foreign
import           Foreign.C
-- import Data.Data

import           Graphics.LibUI.FFI
import           Graphics.LibUI.MonadUI

-- class ToCUIControlIO c where
--     toCUIControlIO :: c -> IO CUIControl

-- data UIControl c = UIControlWindow (UIWindow c)
--                 | UIControlButton UIButton
--                 | UIControlBox (UIBox c)
--                 | UIControlCheckbox UICheckbox
--                 | UIControlEntry UIEntry
--                 | UIControlLabel UILabel
--                 | UIControlTab (UITab c)
--                 | UIControlGroup (UIGroup c)
--                 | UIControlSpinbox UISpinbox
--                 | UIControlSlider UISlider
--                 | UIControlProgressBar UIProgressBar
--                 | UIControlSeparator UISeparator
--                 | UIControlCombobox UICombobox
--                 | UIControlEditableCombobox UIEditableCombobox
--                 | UIControlRadioButtons UIRadioButtons
--                 | UIControlMultlineEntry UIMultilineEntry
--                 | UIControlMenuItem UIMenuItem
--                 | UIControlMenu UIMenu
--  deriving(Typeable)

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


menu :: String -> [UIMenuItem] -> UI ()
menu name items = UI $ do
    c <- toCUIControlIO $ UIMenu name items
    return ((), [])

group :: String -> UI a -> UI (CUIGroup, a)
group title items = UI $ do
    (x, [c]) <- runUI items
    c <- toCUIIO $ UIGroup title 1 c :: IO CUIGroup
    return ((c, x), [toCUIControl c])

progressbar :: Int -> UI CUIProgressBar
progressbar value = UI $ do
    pg <- toCUIIO (UIProgressBar value)
    return (pg, [toCUIControl pg])

slider :: Int -> Int -> Int -> UI ()
slider value min max = wrap (UISlider value min max)

spinbox :: Int -> Int -> Int -> UI ()
spinbox value min max = wrap (UISpinbox value min max)

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

checkbox :: String -> UI ()
checkbox t = wrap (UICheckbox False t)

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
    cui@(CUIControl ptr) <- toCUIControlIO (UILabel t)
    return (CUILabel (castPtr ptr), [cui])

entry :: String -> UI ()
entry t = wrap (UIEntry False t)

searchEntry :: String -> UI ()
searchEntry t = wrap (UISearchEntry False t)

passwordEntry :: String -> UI ()
passwordEntry t = wrap (UISearchEntry False t)

form cs = wrap (UIForm cs)
formItem x e = (x, e)

-- stuff = runUILoop ui
--   where
--     ui = UI $ do
--         let window = def { uiWindowTitle = "Hey there"
--                          , uiWindowChild = def { uiBoxChildren = [ UIBoxChild False
--                                                                               (UILabel "Hey")
--                                                                  ]
--                                                }
--                          , uiWindowOnClosing = Just $ print "Bye!"
--                          }
--         c <- toCUIControlIO window
--         return ((), [ c ])

stuff :: IO ()
stuff = runUILoop ui
  where
    ui :: UI ()
    ui = do
        menu "File" [ "Open"
                    , "Save"
                    , UIMenuItemQuit
                    ]
        void $ window "libui Control Gallery" 640 300 True $
            void $ tabs $ do
                tab "Basic Controls" $ do
                    hbox $
                        -- button "Button"
                        checkbox "Checkbox"
                    label "This is a label. Right now, labels can only span one line."
                    void $ group "Entries" $
                        form [ formItem "Entry" (entry "")
                             , formItem "Entry" (entry "")
                             , formItem "Search Entry" (searchEntry "")
                             ]
                tab "Basic Controls" $ hbox $ do
                    void $ group "Numbers" (return ())
                    void $ group "Lists" (return ())
                tab "Data Choosers" mempty

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

instance {-# OVERLAPPING #-} ToCUIControlIO UIButton where
    toCUIControlIO UIButton{..} = toCUIControl <$> do
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

instance {-# OVERLAPS #-} ToCUIControlIO UICheckbox where
    toCUIControlIO UICheckbox{..} = do
        c <- c_uiNewCheckbox =<< newCString uiCheckboxText
        c_uiCheckboxSetChecked c (if uiCheckboxChecked then 1 else 0)
        return (toCUIControl c)

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
    return (toCUIControl e)

instance {-# OVERLAPS #-} ToCUIControlIO UIEntry where
    toCUIControlIO e@UIEntry{} = mkEntry c_uiNewEntry e
    toCUIControlIO e@UIPasswordEntry{} = mkEntry c_uiNewPasswordEntry e
    toCUIControlIO e@UISearchEntry{} = mkEntry c_uiNewSearchEntry e

-- ** Labels
data UILabel = UILabel { uiLabelText :: String
                       }

instance Default UILabel where
    def = UILabel { uiLabelText = ""
                  }

instance {-# OVERLAPS #-} ToCUIControlIO UILabel where
    toCUIControlIO UILabel{..} =
        toCUIControl <$> (c_uiNewLabel =<< newCString uiLabelText)

-- ** Text Forms
data UIForm = UIForm [(String, UI ())]

instance {-# OVERLAPS #-} ToCUIControlIO UIForm where
    toCUIControlIO (UIForm cs) = do
        f <- c_uiNewForm
        c_uiFormSetPadded f 10
        forM_ cs $ \(n, c) -> do
            n' <- newCString n
            (_, [c']) <- runUI c
            c_uiFormAppend f n' c' 1
        return (toCUIControl f)

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

instance {-# OVERLAPS #-} ToCUIControlIO UISpinbox where
    toCUIControlIO UISpinbox{..} = do
        sb <- c_uiNewSpinbox (fromIntegral uiSpinboxMin) (fromIntegral uiSpinboxMax)
        c_uiSpinboxSetValue sb (fromIntegral uiSpinboxValue)
        -- c_uiProgressBarSetValue pb (fromIntegral uiProgressBarValue)
        return (toCUIControl sb)

data UISlider = UISlider { uiSliderValue :: Int
                         , uiSliderMin   :: Int
                         , uiSliderMax   :: Int
                         }

instance {-# OVERLAPS #-} ToCUIControlIO UISlider where
    toCUIControlIO UISlider{..} = do
        s <- c_uiNewSlider (fromIntegral uiSliderMin) (fromIntegral uiSliderMax)
        c_uiSliderSetValue s (fromIntegral uiSliderValue)
        return (toCUIControl s)

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

instance {-# OVERLAPS #-} ToCUIControlIO UISeparator where
    toCUIControlIO UIHorizontalSeparator = toCUIControl <$> c_uiNewHorizontalSeparator
    toCUIControlIO UIVerticalSeparator = toCUIControl <$> c_uiNewVerticalSeparator

-- ** Selects
data UICombobox = UICombobox { uiComboboxSelected :: Bool
                             }

instance {-# OVERLAPS #-} ToCUIControlIO UICombobox where
    toCUIControlIO UICombobox{..} = do
        cb <- c_uiNewCombobox
        c_uiComboboxSetSelected cb (if uiComboboxSelected then 1 else 0)
        return (toCUIControl cb)

data UIEditableCombobox = UIEditableCombobox { uiEditableComboboxText :: String
                                             }

instance {-# OVERLAPS #-} ToCUIControlIO UIEditableCombobox where
    toCUIControlIO UIEditableCombobox{..} = do
        cb <- c_uiNewEditableCombobox
        c_uiEditableComboboxSetText cb =<< newCString uiEditableComboboxText
        return (toCUIControl cb)

data UIRadioButtons = UIRadioButtons { uiRadioButtonsSelected :: Int
                                     }

instance {-# OVERLAPS #-} ToCUIControlIO UIRadioButtons where
    toCUIControlIO UIRadioButtons{..} =
        toCUIControl <$> c_uiNewRadioButtons

-- ** Textarea
data UIMultilineEntry = UIMultilineEntry { uiMultilineEntryText     :: String
                                         , uiMultilineEntryReadOnly :: Bool
                                         }

instance {-# OVERLAPS #-} ToCUIControlIO UIMultilineEntry where
    toCUIControlIO UIMultilineEntry{..} =
        toCUIControl <$> c_uiNewMultilineEntry

-- ** Menus

-- |
-- The application menu. Either a window menu, as in Windows/Linux or the top
-- bar menu in OSX.
--
-- Renders with `uiMenu` 'c_uiNewMenu', using 'CUIMenu'
data UIMenu = UIMenu { uiMenuName  :: String
                     , uiMenuItems :: [UIMenuItem]
                     }

instance {-# OVERLAPS #-} ToCUIControlIO UIMenu where
    toCUIControlIO UIMenu{..} = do
        m <- c_uiNewMenu =<< newCString uiMenuName
        forM_ uiMenuItems $ \item ->
            appendMenuItem m item
        return $ toCUIControl m

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

