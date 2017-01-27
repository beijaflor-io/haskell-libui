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

showText c = uiQueueMain . setText c . show

clickSource c = liftIO $ do
    esclick <- newAddHandler
    c `onClick` fire esclick c
    return esclick

main :: IO ()
main = do
    escounter <- newAddHandler

    runUILoop $ mdo
        (wnd, _) <- window' def { uiWindowTitle = "Click to resize"
                                      , uiWindowWidth = 200
                                      , uiWindowHeight = 100
                                      , uiWindowChild = vbox $ mdo
                                              btn <- counterButton def
                                              return ()
                                      }
        wrap wnd

        return ()

-- newtype AddHandler = AddHandler { register :: Handler a -> IO (IO ()) }
-- type Handler = a -> IO ()
type EventSource a = (AddHandler a, Handler a)

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd
