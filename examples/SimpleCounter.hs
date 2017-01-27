import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
import           Graphics.LibUI

main :: IO ()
main = do
    uiInit

    opChan <- newChan :: IO (Chan Int)
    addBtn <- uiNewButton "Add"
    subBtn <- uiNewButton "Subtract"
    cntLbl <- uiNewLabel "Count: 0"

    addBtn `onClick` writeChan opChan 1
    subBtn `onClick` writeChan opChan (-1)

    let go opChan count = do
            op <- readChan opChan
            let count' = count + op
            cntLbl `setText` ("Count: " ++ show count')
            go opChan count'

    forkIO $ go opChan 0

    wn <- uiNewWindow "SimpleCounter.hs" 220 100 True
    vb <- uiNewVerticalBox

    wn `setMargined` True
    wn `setChild` vb

    vb `appendChild` addBtn
    vb `appendChild` subBtn
    vb `appendChild` cntLbl

    wn `onClosing` uiQuit
    uiOnShouldQuit (uiQuit >> return 0)

    uiShow wn

    uiMainSteps
    whileM_ getHasMain $ do
        h <- uiMainStep 0
        when (h == 0) $ threadDelay (1000 * 1000 * 16)
