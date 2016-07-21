import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
import           Graphics.LibUI

main :: IO ()
main = do
    uiInit

    webview <- uiNewWebview
    webview `loadHtml` ("<h1>Hello</h1>", "")

    wn <- uiNewWindow "SimpleCounter.hs" 500 500 True
    vb <- uiNewVerticalBox

    wn `setMargined` True
    wn `setChild` vb

    vb `appendChild` webview

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
        print ("JavaScript sent us", r)
    vb `appendChild` btn

    wn `onClosing` uiQuit

    uiWindowCenter wn

    uiOnShouldQuit (uiQuit >> return 0)
    uiShow wn
    uiMain
