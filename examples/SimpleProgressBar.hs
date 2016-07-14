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
        c_uiQuit
        exitSuccess

    wn <- uiNewWindow "Hello World" 680 400 True
    wn `setMargined` True

    vb <- c_uiNewHorizontalBox
    vb `appendChild` pg
    wn `setChild` vb
    wn `onClosing` uiQuit

    uiShow wn
    uiMain
