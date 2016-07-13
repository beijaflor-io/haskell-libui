{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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

import           Graphics.LibUI.FFI
import           Graphics.LibUI.MonadUI

-- |
-- Something that can be rendered with libui
-- class ToCUIControlIO c where
--     toCUIControlIO :: c -> IO CUIControl

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
--                | UIControlMultlineEntry UIMultilineEntry
--                | UIControlMenuItem UIMenuItem
--                | UIControlMenu UIMenu

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
            cb <- castFunPtr <$> c_wrap2 (const (const (void (abort mvRunning))))
            let (CUIControl ptr) = c
            c_uiWindowOnClosing (CUIWindow (castPtr ptr)) cb nullPtr
        cb <- c_wrap1I (const (abort mvRunning))
        c_uiOnShouldQuit cb nullPtr

        c_uiMainSteps
        print "Start"

        loop mvRunning
        takeMVar mvRunning `catch` \UserInterrupt -> do
            print "interrupt"
            void (abort mvRunning)
    loop mvRunning = do
        -- c_uiMain
        e <- isEmptyMVar mvRunning
        when e $ do
            c_uiMainStep 0
            loop mvRunning
    abort mvRunning = do
        print "abort"
        c_uiQuit
        tryPutMVar mvRunning ()
        return 0

window :: String -> Int -> Int -> Bool -> UI () -> UI ()
window title width height hasMenubar child = UI $ do
    c <- toCUIControlIO $ def { uiWindowTitle = title
                              , uiWindowWidth = width
                              , uiWindowHeight = height
                              , uiWindowHasMenubar = hasMenubar
                              , uiWindowChild = child
                              }
    return ((), [c])

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

box
  :: (Num a1, ToCUIControlIO r) =>
     (a1 -> [UIBoxChild CUIControl] -> r) -> UI a -> UI a
box boxtype ui = UI $ do
    (x, cs) <- runUI ui
    c <- toCUIControlIO $ boxtype 1
        (map (UIBoxChild False) cs)
    return (x, [c])


menu :: String -> [UIMenuItem] -> UI ()
menu name items = UI $ do
    c <- toCUIControlIO $ UIMenu name items
    return ((), [])

group :: String -> UI () -> UI ()
group title items = UI $ do
    c <- toCUIControlIO $ UIGroup title 1 (vbox items)
    return ((), [c])

progressbar :: Int -> UI ()
progressbar value = wrap (UIProgressBar value)

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
tabs :: Writer [UI String] () -> UI ()
tabs wts = UI $ do
    let ts :: [UI String]
        ts = snd $ runWriter wts
    ts' <- forM ts $ \t -> do
        (r, c:_) <- runUI t
        return (r, c)
    c <- toCUIControlIO (UITab 1 ts')
    return ((), [c])

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
        window "libui Control Gallery" 640 300 True $
            tabs $ do
                tab "Basic Controls" $ do
                    hbox $ do
                        -- button "Button"
                        checkbox "Checkbox"
                    label "This is a label. Right now, labels can only span one line."
                    group "Entries" $
                        form [ formItem "Entry" (entry "")
                             , formItem "Entry" (entry "")
                             , formItem "Search Entry" (searchEntry "")
                             ]
                tab "Basic Controls" $ hbox $ do
                    group "Numbers" (return ())
                    group "Lists" (return ())
                tab "Data Choosers" mempty

-- ** Windows
instance {-# OVERLAPS #-} ToCUIControlIO a => ToCUIControlIO [a] where
    toCUIControlIO cs = toCUIControl <$> do
        vb <- c_uiNewVerticalBox
        forM_ cs $ \c -> do
            c' <- toCUIControlIO c
            c_uiBoxAppend vb c' 1
        return vb

instance {-# OVERLAPS #-} ToCUIControlIO (UI a) where
    toCUIControlIO ui = do
        (_, cs) <- runUI ui
        toCUIControlIO cs

data UIWindow c =
    UIWindow { uiWindowTitle                :: String
             , uiWindowWidth                :: Int
             , uiWindowHeight               :: Int
             , uiWindowHasMenubar           :: Bool
             , uiWindowMargin               :: Int
             , uiWindowChild                :: c
             , uiWindowOnContentSizeChanged :: Maybe ((Int, Int) -> IO ())
             , uiWindowOnClosing            :: Maybe (IO ())
             }

instance Default (UIWindow c) where
    def = UIWindow { uiWindowTitle = "haskell-libui"
                   , uiWindowWidth = 680
                   , uiWindowHeight = 300
                   , uiWindowHasMenubar = True
                   , uiWindowMargin = 0
                   , uiWindowOnClosing = Nothing
                   , uiWindowOnContentSizeChanged = Nothing
                   , uiWindowChild = error "uiWindowChild needs to be overwritten"
                   }

instance {-# OVERLAPS #-} ToCUIControlIO a => ToCUIControlIO (UIWindow a) where
    toCUIControlIO UIWindow{..} = do
        w <- join $ c_uiNewWindow
            <$> newCString uiWindowTitle
            <*> return (fromIntegral uiWindowWidth)
            <*> return (fromIntegral uiWindowHeight)
            <*> return (if uiWindowHasMenubar then 1 else 0)

        maybe
            (return ())
            (\onClosing -> do
                 cb <- c_wrap2 (\_ _ -> onClosing)
                 c_uiWindowOnClosing w (castFunPtr cb) nullPtr)
            uiWindowOnClosing
        c_uiWindowSetMargined w (fromIntegral uiWindowMargin)

        c <- toCUIControlIO uiWindowChild
        c_uiWindowSetChild w c
        return $ toCUIControl w

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
data UIBox c = UIHorizontalBox { uiBoxPadding  :: Int
                               , uiBoxChildren :: [UIBoxChild c]
                               }
             | UIVerticalBox { uiBoxPadding  :: Int
                             , uiBoxChildren :: [UIBoxChild c]
                             }

instance Default (UIBox c) where
    def = UIVerticalBox { uiBoxPadding = 1
                        , uiBoxChildren = []
                        }

data UIBoxChild c = UIBoxChild { uiBoxChildStretchy :: Bool
                               , uiBoxChildControl  :: c
                               }

instance {-# OVERLAPS #-} ToCUIControlIO c => ToCUIControlIO (UIBox c) where
    toCUIControlIO ui = do
        b <- case ui of
            UIVerticalBox{} -> c_uiNewVerticalBox
            UIHorizontalBox{} -> c_uiNewHorizontalBox
        c_uiBoxSetPadded b (fromIntegral (uiBoxPadding ui))
        forM_ (uiBoxChildren ui) $ \UIBoxChild{..} -> do
            uiBoxChildControl' <- toCUIControlIO uiBoxChildControl
            let uiBoxChildStretchy' = if uiBoxChildStretchy then 1 else 0
            c_uiBoxAppend b uiBoxChildControl' uiBoxChildStretchy'
        return (toCUIControl b)

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

instance {-# OVERLAPS #-} ToCUIControl c => ToCUIControlIO (UITab c) where
    toCUIControlIO UITab{..} = do
        t <- c_uiNewTab
        forM_ uiTabChildren $ \(n, c) -> do
            print n
            n' <- newCString n
            c' <- toCUIControlIO c
            c_uiTabAppend t n' c'
        -- c_uiTabSetMargined t (fromIntegral uiTabMargin) 0
        return (toCUIControl t)

-- ** Groups
data UIGroup c =
    UIGroup { uiGroupTitle  :: String
            , uiGroupMargin :: Int
            , uiGroupChild  :: c
            }

instance {-# OVERLAPS #-} ToCUIControlIO c => ToCUIControlIO (UIGroup c) where
    toCUIControlIO UIGroup{..} = do
        g <- c_uiNewGroup =<< newCString uiGroupTitle
        c_uiGroupSetMargined g (fromIntegral uiGroupMargin)
        child <- toCUIControlIO uiGroupChild
        c_uiGroupSetChild g child
        return (toCUIControl g)

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

instance {-# OVERLAPS #-} ToCUIControlIO UIProgressBar where
    toCUIControlIO UIProgressBar{..} = do
        pb <- c_uiNewProgressBar
        c_uiProgressBarSetValue pb (fromIntegral uiProgressBarValue)
        return (toCUIControl pb)

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

