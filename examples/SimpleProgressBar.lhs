This is a "raw" API demo.

The API exposed by `Graphics.LibUI.FFI` follows the imperative spirit with some
type-class sugars, but essentially is just throwing pointers around on the IO
monad.

Since this example involves a background-thread with a timer, that updates the
progress-bar's value, we import `Control.Concurrent`.

> import Control.Concurrent (forkIO, threadDelay)

We also import `forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()`

> import Control.Monad (forM_)

`Graphics.LibUI.FFI` is the entry-point for the raw FFI module. It exports both
the raw C API, as well as Haskell typed, type-class structured helpers.

> import Graphics.LibUI.FFI

> main :: IO ()
> main = do

`uiInit` needs to be called to initialize `libui` before calling any other FFI
functions.

>     uiInit

We create our `CUILabel` and `CUIProgressBar` controls

>     lb <- uiNewLabel "Starting"
>     pg <- uiNewProgressBar

`forkIO` forks a thread to iterate through the numbers from 0 to 100, waiting
100ms between iterations and updating the progress-bar and label's contents.

>     forkIO $ do
>         forM_ [0..100] $ \i -> do
>             threadDelay (1000 * 100)

`uiQueueMain` is necessary when the code is multi-threaded. Haskell may or may
not be using the `-threaded` RTS, but this should be used unless the updates
are inside callbacks, in which case they'll be executed by the main C thread.

>             uiQueueMain $ do

As you can see throughout, the code is based on abstract type-classes. This
makes it easier to write more generic code.

>                 lb `setText` (show i ++ "% Done")
>                 pg `setValue` i
>         uiQuit

We create the layout for the controls with two layout controls in `Graphics.LibUI.FFI`:
- `CUIBox`, exposed from the module in `uiNewVerticalBox`
  and `uiNewHorizontalBox` variants
- `CUIWindow`, exposed through `uiNewWindow`

>     hb <- uiNewVerticalBox
>     hb `appendChild` lb
>     hb `appendChild` pg
>     wn <- uiNewWindow "SimpleProgressBar.hs" 300 100 True
>     wn `setMargined` True
>     wn `onClosing` uiQuit
>     wn `setChild` hb

>     uiOnShouldQuit (uiQuit >> return 0)
>     uiShow wn
>     uiMain
