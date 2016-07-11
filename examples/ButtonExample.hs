import Graphics.LibUI
import Foreign.C

main :: IO ()
main = do
    title <- newCString "haskell-libui - Control Gallery"
    win <- c_uiNewWindow title 640 480 1
    btn <- c_uiNewButton =<< newCString "Here I am"
    c_uiWindowSetChild win btn
    c_uiWindowSetMargined win 10
    c_uiControlShow win
    c_uiMain
