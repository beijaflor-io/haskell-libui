import Graphics.LibUI
main = do
    uiInit
    wn <- uiNewWindow "Haskell on GUIs" 220 100 True
    btn <- uiNewButton "Click me"
    wn `setChild` btn
    wn `setMargined` True
    uiShow wn
    uiMain
