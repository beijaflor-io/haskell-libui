import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
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

main :: IO ()
main = do
    uiInit

    webview <- uiNewWebview

    wn <- uiNewWindow "SimpleCounter.hs" 600 500 True
    hb <- uiNewHorizontalBox

    wn `setMargined` True
    wn `setChild` hb

    cr <- uiNewCombobox
    cr `appendOptions` (map fst readers)
    cr `setValue` 2

    lhb <- uiNewVerticalBox
    lhb `setPadded` True
    me <- uiNewMultilineEntry

    lhb `appendChild` cr
    lhb `appendChildStretchy` me
    hb `appendChildStretchy` lhb
    sep <- uiNewHorizontalSeparator
    hb `appendChild` sep

    cw <- uiNewCombobox
    cw `appendOptions` (map fst writers)
    cw `setValue` 8

    rhb <- uiNewVerticalBox
    rhb `setPadded` True
    rhb `appendChild` cw
    rhb `appendChildStretchy` webview
    hb `appendChildStretchy` rhb

    let runRender' = runRender cr cw me webview
    me `onChange` runRender'
    cr `onChange` runRender'
    cw `onChange` runRender'

    -- webview `onLoad` do
    --     void $ webview `evalJs`
    --         unlines [ "(function() {"
    --                 , "  $(function() {"
    --                 , "    var button = $('button');"
    --                 , "    button.click(function() {"
    --                 , "      button.text('Click (' + Math.round(new Date().getTime() / 1000) + ')');"
    --                 , "    });"
    --                 , "  });"
    --                 , "  return 'OK';"
    --                 , "})();"
    --                 ]

    -- btn <- uiNewButton "Click to evaluate JS"
    -- btn `onClick` do
    --     void $ webview `evalJs`
    --         unlines [ "(function() {"
    --                 , "  var now = new Date().getTime();"
    --                 , "  var log = document.createElement('div');"
    --                 , "  log.className = 'container';"
    --                 , "  var pre = document.createElement('pre');"
    --                 , "  var code = document.createElement('code');"
    --                 , "  code.appendChild("
    --                 , "    document.createTextNode('Hello Haskell (' + Math.round(now / 1000) + ')')"
    --                 , "  );"
    --                 , "  pre.appendChild(code);"
    --                 , "  log.appendChild(pre);"
    --                 , "  document.body.appendChild(log);"
    --                 , "  return now;"
    --                 , "})()"
    --                 ]
    -- vb `appendChild` btn

    wn `onClosing` uiQuit

    uiWindowCenter wn

    uiOnShouldQuit (uiQuit >> return 0)
    uiShow wn
    uiMain
