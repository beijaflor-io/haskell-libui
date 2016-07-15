import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Foreign.Ptr
import Graphics.LibUI.FFI
import System.Exit
import System.IO

main = do
    hSetBuffering stdout NoBuffering

    uiInit

    lb <- uiNewLabel "Starting"
    pg <- uiNewProgressBar

    forkIO $ do
        forM_ [0..100] $ \i -> do
            threadDelay (1000 * 100)
            v <- getValue pg
            uiQueueMain $ do
                setText lb (show i ++ "% Done")
                setValue pg i
        uiQuit

    hb <- uiNewVerticalBox
    hb `appendChild` lb
    hb `appendChild` pg

    wn <- uiNewWindow "SimpleProgressBar.hs" 300 100 True
    wn `setMargined` True
    wn `onClosing` uiQuit
    wn `setChild` hb
    uiOnShouldQuit (uiQuit >> return 0)

    uiShow wn
    uiMainSteps
    whileM getHasMain $ do
        h <- uiMainStep 0
        when (h == 0) $ threadDelay (1000 * 1000 * 16)
