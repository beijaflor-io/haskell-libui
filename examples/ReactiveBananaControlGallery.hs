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

    runUILoop $ wrap def { uiWindowTitle = "Control Gallery"
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
                                      network <- setupNetwork escounter (btn, counter, time)
                                      actuate network
                                  return ()
                 }

{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
-- Set up the program logic in terms of events and behaviors.
setupNetwork escounter (btn, counter, time) = compile $ do
    ecounter <- fromAddHandler (addHandler escounter)
    ecount <- accumE 0 $ (+1) <$ ecounter
    reactimate $ fmap updateUI ecount
  where
    updateUI currentCount = do
        currentTime <- getCurrentTime
        time `setText` ("Simple UI at " ++ show currentTime)
        counter `setText` show currentCount
        btn `setText` ("Should be simple " ++ show currentCount)
