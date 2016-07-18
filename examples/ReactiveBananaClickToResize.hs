{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Debug.Trace
import Control.Concurrent
import Control.Monad
import Data.Time
import Graphics.LibUI

import Reactive.Banana
import Reactive.Banana.Frameworks

-- | A button that updates it's text to reflect how many times it's been clicked
counterButton :: UIButton -> UI CUIButton
counterButton opts = do
    btn <- button opts { uiButtonText = "0" }
    escounter <- clickSource btn
    liftIO $ do
        network <- compile $ do
            ecounter <- fromAddHandler (addHandler escounter)
            ecount <- accumE 0 $ (+1) <$ ecounter
            reactimate $ showText btn <$> ecount
        actuate network
    return btn
  where
    updateUI btn currentCount =
        btn `setText` (uiButtonText opts ++ " " ++ show currentCount)

showText c = setText c . show

clickSource c = liftIO $ do
    esclick <- newAddHandler
    c `onClick` fire esclick c
    return esclick

positionSource w = liftIO $ do
    esposition <- newAddHandler
    w `onPositionChanged` do
        p <- getPosition w
        fire esposition p
    return (addHandler esposition)

positionIntervalSource :: CUIWindow -> MomentIO (Event (Int, Int))
positionIntervalSource w = do
    esposition <- liftIO $ do
        esposition <- newAddHandler
        let loop p = do
                p' <- getPosition w
                when (p /= p') $
                    fire esposition p'
                threadDelay (1000 * 10)
                loop p'
        forkIO $ loop (0, 0)
        return esposition
    fromAddHandler (addHandler esposition) -- >>= changes

main :: IO ()
main = do
    escounter <- newAddHandler

    runUILoop $ mdo
        (wnd, counter) <- window' def { uiWindowTitle = "Click to resize"
                                      , uiWindowWidth = 200
                                      , uiWindowHeight = 100
                                      , uiWindowChild = vbox $ mdo
                                              time <- label "Window position"
                                              counter <- label ""
                                              btn <- counterButton def
                                              return counter
                                      }
        wrap wnd

        liftIO $ do
            uiWindowCenter wnd
            network <- compile $ do
                epositionInterval <- positionIntervalSource wnd
                reactimate $ showText counter <$> epositionInterval
            actuate network

        return ()

-- newtype AddHandler = AddHandler { register :: Handler a -> IO (IO ()) }
-- type Handler = a -> IO ()
type EventSource a = (AddHandler a, Handler a)

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

-- setupNetwork escounter (wnd, btn, counter, time) = compile $ do
--     ecounter <- fromAddHandler (addHandler escounter)
--     ecount <- accumE 0 $ (+1) <$ ecounter
--     reactimate $ fmap updateUI ecount
--   where
--     updateUI currentCount = do
--         currentTime <- getCurrentTime
--         wnd `setContentSize` (800, 800)
--         time `setText` ("Simple UI at " ++ show currentTime)
--         counter `setText` show currentCount
--         btn `setText` ("Should be simple " ++ show currentCount)

-- dummyIntervalSource :: CUIWindow -> MomentIO (Event (Future (Int, Int)))
-- dummyIntervalSource _ = do
--     esposition <- liftIO $ do
--         esposition <- newAddHandler
--         let loop = do
--                 fire esposition (0, 0)
--                 threadDelay (1000 * 10)
--                 loop
--         forkIO loop
--         return esposition
--     fromChanges (0, 0) (addHandler esposition) >>= changes
