import           Control.Monad
import           Graphics.LibUI
import           Text.Pandoc
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack, unpack)

runRender cr cw me webview = do
    r <- getValue cr
    w <- getValue cw
    e <- getText me

    when (r /= -1 && w /= -1) $ do
        einp <- case (readers !! r) of
                    (_, StringReader rf) -> rf def e
                    (_, ByteStringReader rf) -> do
                        ei <- rf def (ByteString.pack e)
                        return $ fmap fst ei
        case einp of
            Left err -> print err
            Right inp -> do
                out <- case (writers !! w) of
                           (_, PureStringWriter wf) ->
                               return (wf def inp)
                           (_, IOStringWriter wf) ->
                               wf def inp
                           (_, IOByteStringWriter wf) ->
                               ByteString.unpack <$> wf def inp
                webview `loadHtml` (out, "")

makeInputs = do
    cr <- uiNewCombobox
    cr `appendOptions` (map fst readers)
    cr `setValue` 2

    lhb <- uiNewVerticalBox
    lhb `setPadded` True
    me <- uiNewMultilineEntry

    lhb `appendChild` cr
    lhb `appendChildStretchy` me
    inpC <- uiNewGroup "Input"
    inpC `setMargined` True
    inpC `setChild` lhb
    return (inpC, cr, me)

makeOutputs = do
    webview <- uiNewWebview
    cw <- uiNewCombobox
    cw `appendOptions` (map fst writers)
    cw `setValue` 8

    rhb <- uiNewVerticalBox
    rhb `setPadded` True
    rhb `appendChild` cw
    rhb `appendChildStretchy` webview
    outC <- uiNewGroup "Output"
    outC `setMargined` True
    outC `setChild` rhb
    return (outC, cw, webview)

uiNewEditMenu = uiNewMenu "Edit" >>= \menu -> do
    uiMenuAppendQuitItem menu
    uiMenuAppendItemWith menu "Undo" "z" "undo:"
    uiMenuAppendItemWith menu "Redo" "r" "redo:"
    uiMenuAppendSeparator menu
    uiMenuAppendItemWith menu "Copy" "c" "copy:"
    uiMenuAppendItemWith menu "Cut" "x" "cut:"
    uiMenuAppendItemWith menu "Paste" "v" "paste:"
    uiMenuAppendItemWith menu "Select All" "a" "selectAll:"

main :: IO ()
main = do
    uiInit

    uiNewEditMenu

    uiNewMenu "Stuff" >>= \menu -> do
        uiMenuAppendItem menu "Open"
        uiMenuAppendItem menu "Save"

    (inpC, cr, me) <- makeInputs
    (outC, cw, webview) <- makeOutputs

    let runRender' = runRender cr cw me webview
    me `onChange` runRender'
    cr `onChange` runRender'
    cw `onChange` runRender'

    hb <- uiNewHorizontalBox
    hb `setPadded` True
    hb `appendChildStretchy` inpC
    hb `appendChildStretchy` outC

    wn <- uiNewWindow "markd - Pandoc on GUIs" 700 500 True
    wn `setChild` hb
    wn `setMargined` True
    wn `onClosing` uiQuit
    uiOnShouldQuit $ do
        uiQuit
        return 1

    uiWindowCenter wn
    uiShow wn
    uiMain
