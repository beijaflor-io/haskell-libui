import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
import           Data.Maybe

import           Graphics.LibUI.FFI

makeBasicControlsTab :: IO CUIBox
makeBasicControlsTab = do
    vb <- uiNewVerticalBox
    vb `setPadded` True

    hb <- uiNewHorizontalBox
    hb `setPadded` True
    vb `appendChild` hb

    hb `appendIOChild` uiNewButton "Button"
    hb `appendIOChild` uiNewCheckbox "Checkbox"
    vb `appendIOChild` uiNewLabel "This is a label. Right now, labels can only span one line"
    vb `appendIOChild` uiNewHorizontalSeparator

    group <- uiNewGroup "Entries"
    group `setMargined` True
    vb `appendChild` group

    entryForm <- uiNewForm
    entryForm `setPadded` True
    group `setChild` entryForm

    entry <- uiNewEntry
    entryForm `appendInput` ("Entry", entry)
    passwordEntry <- uiNewPasswordEntry
    entryForm `appendInput` ("Password Entry", passwordEntry)
    searchEntry <- uiNewSearchEntry
    entryForm `appendInput` ("Search Entry", searchEntry)
    multilineEntry <- uiNewMultilineEntry
    entryForm `appendInput` ("Multiline Entry", multilineEntry)
    nonWrappingMultilineEntry <- uiNewNonWrappingMultilineEntry
    entryForm `appendInput` ("NonWrappingMultiline Entry", nonWrappingMultilineEntry)

    return vb

makeNumbersTab :: IO CUIBox
makeNumbersTab = do
    hb <- uiNewHorizontalBox
    hb `setPadded` True

    numbersGroup <- uiNewGroup "Numbers"
    numbersGroup `setMargined` True

    hb `appendChildStretchy` numbersGroup

    numbersGroupVb <- uiNewVerticalBox
    numbersGroupVb `setPadded` True
    numbersGroup `setChild` numbersGroupVb

    spinbox <- uiNewSpinbox 0 100
    numbersGroupVb `appendChild` spinbox
    slider <- uiNewSlider 0 100
    numbersGroupVb `appendChild` slider
    pbar <- uiNewProgressBar
    numbersGroupVb `appendChild` pbar

    spinbox `onChange` do
        svalue <- getValue spinbox
        pbar `setValue` svalue
    slider `onChange` do
        svalue <- getValue slider
        pbar `setValue` svalue

    ipbar <- uiNewProgressBar
    ipbar `setValue` (-1)
    numbersGroupVb `appendChild` ipbar

    listsGroup <- uiNewGroup "Lists"
    listsGroup `setMargined` True
    hb `appendChildStretchy` listsGroup

    listsGroupVb <- uiNewVerticalBox
    listsGroupVb `setPadded` True
    listsGroup `setChild` listsGroupVb
    combobox <- uiNewCombobox
    combobox `appendOptions` [ "Combobox Item 1"
                             , "Combobox Item 2"
                             , "Combobox Item 3"
                             ]
    listsGroupVb `appendChild` combobox

    ecombobox <- uiNewEditableCombobox
    ecombobox `appendOptions` [ "Editable Combobox Item 1"
                              , "Editable Combobox Item 2"
                              , "Editable Combobox Item 3"
                              ]
    listsGroupVb `appendChild` ecombobox

    rb <- uiNewRadioButtons
    rb `appendOptions` [ "Radio Button 1"
                       , "Radio Button 2"
                       , "Radio Button 3"
                       ]
    listsGroupVb `appendChild` rb

    return hb

makeDataChoosersTab :: CUIWindow -> IO CUIBox
makeDataChoosersTab window = do
    hb <- uiNewHorizontalBox
    hb `setPadded` True

    vbLeft <- uiNewVerticalBox
    vbLeft `setPadded` True
    hb `appendChild` vbLeft

    vbLeft `appendIOChild` uiNewDatePicker
    vbLeft `appendIOChild` uiNewTimePicker
    vbLeft `appendIOChild` uiNewDateTimePicker

    vbLeft `appendIOChild` uiNewFontButton
    vbLeft `appendIOChild` uiNewColorButton

    hb `appendIOChild` uiNewVerticalSeparator

    vbRight <- uiNewVerticalBox
    vbRight `setPadded` True
    hb `appendChildStretchy` vbRight

    grid <- uiNewGrid
    grid `setPadded` True
    vbRight `appendChild` grid

    openFileBtn <- uiNewButton "Open file"
    openFileEntry <- uiNewEntry
    openFileEntry `setReadOnly` True

    openFileBtn `onClick` do
        mfp <- uiOpenFile window
        let fp = fromMaybe "(cancelled)" mfp
        openFileEntry `setText` fp

    uiGridAppend grid openFileBtn 0 0 1 1 0 UIAlignFill 0 UIAlignFill
    uiGridAppend grid openFileEntry 1 0 1 1 1 UIAlignFill 0 UIAlignFill

    saveFileBtn <- uiNewButton "Save file"
    saveFileEntry <- uiNewEntry
    saveFileEntry `setReadOnly` True
    saveFileBtn `onClick` do
        mfp <- uiSaveFile window
        case mfp of
            Just fp -> uiMsgBox window "File selected (don't worry, it's still there)" fp
            Nothing -> uiMsgBoxError window "No file selected" "Don't be alarmed!"


    uiGridAppend grid saveFileBtn 0 1 1 1 0 UIAlignFill 0 UIAlignFill
    uiGridAppend grid saveFileEntry 1 1 1 1 1 UIAlignFill 0 UIAlignFill

    msgGrid <- uiNewGrid
    msgGrid `setPadded` True
    uiGridAppend grid msgGrid 0 2 2 1 0 UIAlignCenter 0 UIAlignStart

    msgBtn <- uiNewButton "Message box"
    msgBtn `onClick`
        uiMsgBox window "This is a normal message box." "More detailed information can be shown here."
    uiGridAppend msgGrid msgBtn 0 0 1 1 0 UIAlignFill 0 UIAlignFill

    errBtn <- uiNewButton "Error box"
    errBtn `onClick`
        uiMsgBoxError window "This is a normal message box." "More detailed information can be shown here."
    uiGridAppend msgGrid errBtn 1 0 1 1 0 UIAlignFill 0 UIAlignFill

    return hb

main :: IO ()
main = do
    uiInit
    wn <- uiNewWindow "haskell-libui - Simple Control Gallery" 640 480 True
    wn `onClosing` uiQuit
    uiOnShouldQuit $ do
        uiQuit
        return 0

    tabs <- uiNewTabs
    wn `setChild` tabs
    wn `setMargined` True

    basicControlsTab <- makeBasicControlsTab
    tabs `appendTab` ("Basic Controls", basicControlsTab)
    (tabs, 0 :: Int) `setMargined` True
    numbersTab <- makeNumbersTab
    tabs `appendTabMargined` ("Numbers and Lists", numbersTab)
    dataChoosersTab <- makeDataChoosersTab wn
    tabs `appendTabMargined` ("Data Choosers", dataChoosersTab)

    uiShow wn

    uiMainSteps
    whileM_ getHasMain $ do
        h <- uiMainStep 0
        when (h == 0) $ threadDelay (1000 * 1000 * 16)
