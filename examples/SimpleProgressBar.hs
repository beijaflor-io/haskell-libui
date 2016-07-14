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
            q <- c_wrap1 (\_ -> (setValue pg i))
            c_uiQueueMain q nullPtr
        c_uiQuit
        exitSuccess

    wn <- uiNewWindow "Hello World" 680 400 True
    vb <- c_uiNewHorizontalBox
    vb `appendChild` pg
    wn `setChild` vb
    wn `onClosing` c_uiQuit

    -- fmenu <- uiNewMenu "file"
    -- c_uiMenuAppendQuitItem fmenu

    uiShow wn
    c_uiMainSteps
    forever $
        c_uiMainStep 0
