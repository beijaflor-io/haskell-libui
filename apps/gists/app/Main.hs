{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack, unpack)
import qualified Data.Text                  as Text (unpack)
import           Graphics.LibUI
import           Graphics.LibUI.OSX
import           Network.Wreq

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

makeGist :: Value -> IO CUIGroup
makeGist gist = do
    g <- uiNewGroup (Text.unpack (gist ^. key "url" . _String))
    l <- uiNewLabel (Text.unpack (gist ^. key "created_at" . _String))
    g `setChild` l
    return g

makeWindow gistsC (FileMenu n o s sa) = do
    hb <- uiNewHorizontalBox
    hb `setPadded` True

    hb `appendIOChild` makeControls

    g <- uiNewGroup "Gists"
    vb <- uiNewVerticalBox
    -- me <- uiNewMultilineEntry
    -- vb `appendChildStretchy` me
    g `setChild` vb
    hb `appendChild` g

    wn <- uiNewWindow "gists" 700 500 True
    wn `setChild` hb
    wn `setMargined` True
    wn `onClosing` uiQuit

    uiOnShouldQuit $ do
        uiQuit
        return 1

    forkIO $ forever $ do
        gists <- readChan gistsC
        uiQueueMain $ do
            forM_ gists $ \gist -> do
                gist <- makeGist (head gists)
                vb `appendChild` gist
            print gists
            -- me `setText` show gists

    return wn

getGists :: String -> IO [Value]
getGists str = do
    res <- get "https://api.github.com/gists" >>= asJSON
    return $ res ^. responseBody

main :: IO ()
main = do
    uiInit

    gistsC <- newChan
    forkIO $ do
        gists <- getGists "yamadapc"
        writeChan gistsC gists

    fileMenu <- uiNewFileMenu
    uiNewEditMenu

    wn <- makeWindow gistsC fileMenu

    uiShow wn
    uiMain
