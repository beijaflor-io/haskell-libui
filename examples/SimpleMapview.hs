import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
import           Graphics.LibUI
import           Graphics.LibUI.OSX

main :: IO ()
main = do
    uiInit

    mapview <- c_uiNewMapview
    {-forkIO $ do-}
        {-threadDelay (1000 * 1000)-}
        {-uiQueueMain $-}
    print mapview

    wn <- uiNewWindow "SimpleMapview.hs" 600 500 True

    wn `setMargined` False
    putStrLn "[haskell] Appending map..."
    wn `setChild` mapview
    putStrLn "[haskell] Map appended"

    putStrLn "[haskell] Displaying GUI..."
    wn `onClosing` uiQuit
    uiOnShouldQuit (uiQuit >> return 0)
    uiShow wn

    putStrLn "[haskell] GUI displayed..."

    uiMain
