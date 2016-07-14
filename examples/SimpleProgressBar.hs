import Control.Concurrent
import Control.Monad
import Foreign.Ptr
import Graphics.LibUI.FFI
import System.Exit

main = do
    uiInit

    pg <- c_uiNewProgressBar

    forkOn 2 $ do
        forM_ [0..100] $ \i -> do
            threadDelay (1000 * 100)
            v <- getValue pg
            print (v, i)
            uiQueueMain (setValue pg i)
        uiQuit
        exitSuccess

    vb <- c_uiNewHorizontalBox
    vb `appendChild` pg

    wn <- uiNewWindow "Hello World" 680 400 True
    wn `setMargined` True
    wn `onClosing` uiQuit
    wn `setChild` vb

    uiShow wn
    uiMain
