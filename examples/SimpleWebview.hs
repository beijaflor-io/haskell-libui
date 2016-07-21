import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
import           Graphics.LibUI

uiNewLogPanel = do
    e <- uiNewMultilineEntry
    e `setReadOnly` True
    uiSetEnabled e False
    wc <- newChan
    forkIO $ forever $ do
        str <- readChan wc
        uiQueueMain $ do
            e `appendText` str
            e `appendText` "\n"
    return (e, wc)

main :: IO ()
main = do
    uiInit

    webview <- uiNewWebview
    webview `loadHtml` ("<h1>Hello</h1>", "")

    wn <- uiNewWindow "SimpleCounter.hs" 500 500 True

    hb <- uiNewHorizontalBox
    vb <- uiNewVerticalBox

    hb `appendChild` vb
    wn `setMargined` True
    wn `setChild` hb

    vb `appendChild` webview

    (e, wc) <- uiNewLogPanel
    hb `appendChild` e

    btn <- uiNewButton "Click to evaluate JS"
    btn `onClick` do
        r <- webview `evalJs`
            unlines [ "(function() {"
                    , "  var now = new Date().getTime();"
                    , "  document.body.appendChild("
                    , "    document.createElement('br')"
                    , "  );"
                    , "  document.body.appendChild("
                    , "    document.createTextNode('Hello webview from Haskell ' + now)"
                    , "  );"
                    , "  return now;"
                    , "})()"
                    ]
        writeChan wc (show ("JavaScript sent us", r))
    vb `appendChild` btn

    wn `onClosing` uiQuit

    uiWindowCenter wn

    uiOnShouldQuit (uiQuit >> return 0)
    uiShow wn
    uiMain
