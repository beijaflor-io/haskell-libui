import           Control.Concurrent
import           Data.Time
import           Graphics.LibUI

main :: IO ()
main = do
    countM <- newMVar 0 :: IO (MVar Int)
    runUILoop $ do
        wrap def { uiWindowTitle = "Control Gallery"
                 , uiWindowMargin = 100
                 , uiWindowChild = vbox $ do
                     time <- label "Simple UI"
                     counter <- label ""
                     wrap $
                         UIButton "Should be simple" $
                             Just $ do
                                 print time
                                 setText time =<<
                                     (("Simple UI - " ++) . show <$>
                                          getCurrentTime)
                                 setText counter =<<
                                     show <$> modifyMVar countM (\c -> return (c + 1, c + 1))
                 }
