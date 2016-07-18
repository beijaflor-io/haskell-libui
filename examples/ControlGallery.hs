{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Concurrent
import           Data.Time
import           Graphics.LibUI

main :: IO ()
main = runUILoop $
    window' def { uiWindowTitle = "Control Gallery"
                , uiWindowWidth = 200
                , uiWindowChild = vbox $
                    mdo time <- label "Simple UI"
                        countM <- liftIO $ newMVar 0 :: UI (MVar Int)
                        counter <- label ""
                        btn <- button def { uiButtonText = "Should be simple"
                                          , uiButtonOnClicked = Just $
                                              mdo currentTime <- getCurrentTime
                                                  time `setText`
                                                      ("Simple UI at " ++
                                                           show currentTime)
                                                  currentCount <- modifyMVar countM
                                                                             (\c -> return ( c +
                                                                                               1
                                                                                           , c +
                                                                                               1
                                                                                           ))
                                                  counter `setText`
                                                      show currentCount
                                                  btn `setText`
                                                      ("Should be simple " ++
                                                           show currentCount)
                                          }
                        wrap btn
                }
