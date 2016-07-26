import           Control.Monad
import           Graphics.LibUI.FFI
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack, unpack)

data FileMenu c = FileMenu c c c c

uiNewFileMenu = uiNewMenu "File" >>= \menu -> do
    uiMenuAppendQuitItem menu
    FileMenu
        <$> uiMenuAppendItemWithDefaultTarget menu "New" "n" "onClicked:"
        <*> uiMenuAppendItemWithDefaultTarget menu "Open" "o" "onClicked:"
        <*> uiMenuAppendItemWithDefaultTarget menu "Save" "s" "onClicked:"
        <*> uiMenuAppendItemWithDefaultTarget menu "Save As" "S" "onClicked:"

uiNewEditMenu = uiNewMenu "Edit" >>= \menu -> do
    uiMenuAppendItemWith menu "Undo" "z" "undo:"
    uiMenuAppendItemWith menu "Redo" "r" "redo:"
    uiMenuAppendSeparator menu
    uiMenuAppendItemWith menu "Copy" "c" "copy:"
    uiMenuAppendItemWith menu "Cut" "x" "cut:"
    uiMenuAppendItemWith menu "Paste" "v" "paste:"
    uiMenuAppendItemWith menu "Select All" "a" "selectAll:"

makeControls = do
    g <- uiNewGroup "Languages"
    vb <- uiNewVerticalBox
    g `setChild` vb
    let languages = [ "Haskell"
                    , "JavaScript"
                    , "D"
                    ]

    forM_ languages $
        \l -> do
            l' <- uiNewButton l
            vb `appendChild` l'

    return g

makeWindow (FileMenu n o s sa) = do
    hb <- uiNewHorizontalBox
    hb `setPadded` True

    hb `appendIOChild` makeControls

    wn <- uiNewWindow "gists" 700 500 True
    wn `setChild` hb
    wn `setMargined` True
    wn `onClosing` uiQuit

    uiOnShouldQuit $ do
        uiQuit
        return 1

    return wn

main :: IO ()
main = do
    uiInit

    fileMenu <- uiNewFileMenu
    uiNewEditMenu

    wn <- makeWindow fileMenu

    uiWindowCenter wn
    uiShow wn
    uiMain
