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

main :: IO ()
main = do
    uiInit

    wn <- uiNewWindow "markd - Pandoc on GUIs" 600 500 True
    hb <- uiNewHorizontalBox
    hb `setPadded` True

    wn `setMargined` True
    wn `setChild` hb

    (inpC, cr, me) <- makeInputs
    hb `appendChildStretchy` inpC
    (outC, cw, webview) <- makeOutputs
    hb `appendChildStretchy` outC

    let runRender' = runRender cr cw me webview
    me `onChange` runRender'
    cr `onChange` runRender'
    cw `onChange` runRender'

    uiWindowCenter wn
    wn `onClosing` uiQuit
    uiOnShouldQuit (uiQuit >> return 0)
    uiShow wn
    uiMain
