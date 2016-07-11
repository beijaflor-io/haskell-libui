{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.LibUI where

import Foreign.Ptr
import Foreign.C

foreign import ccall "uiMain"
    c_uiMain :: IO ()

foreign import ccall "uiControlShow"
    c_uiControlShow :: Ptr () -> IO ()

foreign import ccall "uiControl"
    c_uiControl :: Ptr () -> IO (Ptr ())

foreign import ccall "uiNewWindow"
    c_uiNewWindow :: CString -> CInt -> CInt -> CInt -> IO (Ptr ())

foreign import ccall "uiNewButton"
    c_uiNewButton :: CString -> IO (Ptr ())

foreign import ccall "uiWindowSetChild"
    c_uiWindowSetChild :: Ptr () -> Ptr () -> IO ()

foreign import ccall "uiWindowSetMargined"
    c_uiWindowSetMargined :: Ptr () -> CInt -> IO ()
