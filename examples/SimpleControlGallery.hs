import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops

import           Graphics.LibUI.FFI

main :: IO ()
main = do
    uiInit

    vb1 <- uiNewVerticalBox
    addBtn <- uiNewButton "Add"
    subBtn <- uiNewButton "Subtract"
    cntLbl <- uiNewLabel "Count: 0"
    vb1 `appendChild` addBtn
    vb1 `appendChild` subBtn
    vb1 `appendChild` cntLbl

    vb2 <- uiNewVerticalBox
    tab2Lbl <- uiNewLabel "Tab 2"
    vb2 `appendChild` tab2Lbl

    tabs <- uiNewTabs
    tabs `appendTab` ("Basic Controls", vb1)
    tabs `appendTab` ("Tab 2", vb2)

    wn <- uiNewWindow "Simple Control Gallery" 640 400 True
    wn `onClosing` uiQuit
    uiOnShouldQuit (uiQuit >> return 0)

    wn `setMargined` True
    wn `setChild` tabs

    uiShow wn
    uiWindowCenter wn

    uiMainSteps
    whileM_ getHasMain $ do
        h <- uiMainStep 0
        when (h == 0) $ threadDelay (1000 * 1000 * 16)
