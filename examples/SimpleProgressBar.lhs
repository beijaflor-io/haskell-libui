This is a "raw" API demo.

The API exposed by 'Graphics.LibUI.FFI' follows the imperative spirit with some
type-class sugars, but essentially is just throwing pointers around on the IO
monad.

> import Control.Concurrent (forkIO, threadDelay)
> import Control.Monad (when, forM_)
> import Control.Monad.Loops (whileM_)
>
> import Graphics.LibUI.FFI
>
> main :: IO ()
> main = do
>     uiInit
>     lb <- uiNewLabel "Starting"
>     pg <- uiNewProgressBar
>     forkIO $ do
>         forM_ [0..100] $ \i -> do
>             threadDelay (1000 * 100)
>             uiQueueMain $ do
>                 setText lb (show i ++ "% Done")
>                 setValue pg i
>         uiQuit
>     hb <- uiNewVerticalBox
>     hb `appendChild` lb
>     hb `appendChild` pg
>     wn <- uiNewWindow "SimpleProgressBar.hs" 300 100 True
>     wn `setMargined` True
>     wn `onClosing` uiQuit
>     wn `setChild` hb
>     uiOnShouldQuit (uiQuit >> return 0)
>     uiShow wn
>     uiMainSteps
>     whileM_ getHasMain $ do
>         h <- uiMainStep 0
>         when (h == 0) $ threadDelay (1000 * 1000 * 16)
