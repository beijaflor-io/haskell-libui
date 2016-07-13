{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Concurrent
import           Data.Time
import           Graphics.LibUI

main :: IO ()
main = do
    runUILoop $ do
        wrap def { uiWindowTitle = "Control Gallery"
                 , uiWindowMargin = 100
                 , uiWindowChild = vbox $ mdo
                     time <- label "Simple UI"
                     countM <- liftIO $ newMVar 0 :: UI (MVar Int)
                     counter <- label ""
                     btn <- button
                         def { uiButtonText = "Should be simple"
                             , uiButtonOnClicked = Just $ do
                                     print time
                                     currentTime <- getCurrentTime
                                     setText time ("Simple UI" ++ show currentTime)
                                     currentCount <- modifyMVar_ countM (\c -> return (c + 1))
                                     setText counter (show currentCount)
                                     setText btn ("Should be simple " ++ show currentCount)
                             }
                     wrap btn
                     return ()
                 }
