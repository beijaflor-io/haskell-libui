import           Control.Concurrent
import           Control.Exception
import           Foreign.C
import           Foreign.Ptr
import           Graphics.LibUI

main :: IO ()
main = stuff
    -- title <- newCString "haskell-libui - Control Gallery"
    -- win <- c_uiNewWindow title 640 480 1
    -- btn <- c_uiNewButton =<< newCString "Here I am"
    -- mcount <- newMVar 0
    -- let onClick b _ = do
    --         count <- modifyMVar mcount (\c -> return (c + 1, c + 1)) :: IO Int
    --         c_uiButtonSetText b =<< newCString (show count)
    -- cb <- c_wrap2 onClick
    -- c_uiButtonOnClicked btn cb nullPtr
    -- c_uiWindowSetChild win btn
    -- c_uiWindowSetMargined win 10
    -- c_uiControlShow win
    -- c_uiMain
