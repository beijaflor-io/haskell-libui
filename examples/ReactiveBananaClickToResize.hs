{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           Graphics.LibUI

import           Reactive.Banana
import           Reactive.Banana.Frameworks

main :: IO ()
main = do
    escounter <- newAddHandler

    runUILoop $ mdo
        wnd <- window' def { uiWindowTitle = "Click to resize"
                           , uiWindowWidth = 200
                           , uiWindowChild = vbox $ mdo
                                   time <- label "Simple UI"
                                   countM <- liftIO $ newMVar 0 :: UI (MVar Int)
                                   counter <- label ""
                                   btn <- button
                                          def { uiButtonText = "Should be simple"
                                              , uiButtonOnClicked = Just (fire escounter ())
                                              }

                                   liftIO $ do
                                       network <- setupNetwork escounter (wnd, btn, counter, time)
                                       actuate network

                                   return ()
                           }
        wrap wnd
        liftIO $ uiWindowCenter wnd
        return ()

-- newtype AddHandler = AddHandler { register :: Handler a -> IO (IO ()) }
-- type Handler = a -> IO ()
type EventSource a = (AddHandler a, Handler a)

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

setupNetwork escounter (wnd, btn, counter, time) = compile $ do
    ecounter <- fromAddHandler (addHandler escounter)
    ecount <- accumE 0 $ (+1) <$ ecounter
    reactimate $ fmap updateUI ecount
  where
    updateUI currentCount = do
        currentTime <- getCurrentTime
        wnd `setContentSize` (800, 800)
        time `setText` ("Simple UI at " ++ show currentTime)
        counter `setText` show currentCount
        btn `setText` ("Should be simple " ++ show currentCount)
