# haskell-libui
**GPLv3**
- - -

Haskell bindings to the [`libui`](https://github.com/andlabs/libui) C library.

The library is currently only an FFI wrapper.

Useful top-level modules are:
- `Graphics.LibUI`
- `Graphics.LibUI.OSX`

They export general and OSX specific functionality in raw C and "wrapped"
Haskell APIs. Currently implemented for OSX only are wrappers for Webkit and a
partial wrapper to Mapviews, both of which have examples. This should go back to
upstream `libui` once it's solid.

**Important**

This package needs some splitting and cleaning-up, as well as some more work,
but the bits exposed by `Graphics.LibUI.FFI` (`Graphics.LibUI.FFI.Wrapped`,
`Graphics.LibUI.FFI.Raw` & OSX variants) should be ok to use.

They aren't used in anything serious right now. You'd be courageous to do it.

## Usage
```haskell
import Graphics.LibUI
main = do
    uiInit
    wn <- uiNewWindow "Haskell on GUIs" 220 100 True
    btn <- uiNewButton "Click me"
    wn `setChild` btn
    wn `setMargined` True
    uiShow wn
    uiMain
```
![](/screenshot.png)
![](/screenshot-linux.png)

You can run this example with:
```
git clone https://github.com/beijaflor-io/haskell-libui
cd haskell-libui
make READMEExample
./READMEExample
```

## Examples
There're several examples on the `examples` directory. The `Simple...` examples
only use the `Graphics.LibUI.FFI` part of the library and are what you want to
look at first.

The `ReactiveBanana...` examples are playgrounds for showing how you could wrap
the callback based API with FRP without much work.

### markd - A simple GUI for Pandoc using a webview
`apps/markd` is an example application (currently only working on OSX), which
previews how a file is rendered by `pandoc`.
![](https://www.dropbox.com/s/oxva6l87qjg24yf/Screenshot%202016-08-16%2000.15.01.png?dl=1) 

- - -
## Roadmap

- [x] Raw FFI available in `Graphics.LibUI.FFI.Raw`
  * [x] All functions from the FFI mirror their `libui` names prefixed with `c_`
  * [x] Document the FFI
- [x] Haskell callback based API in `Graphics.LibUI.FFI.Wrapped`
  * [x] Data-wrappers for `uiControl` type
  * [x] Callback based API
- [ ] Higher-level API on `examples` and `Graphics.LibUI.Types` and
      `Graphics.LibUI.MonadUI`
  * [x] Building concrete representations of UI controls based on declarative
        code
  * [ ] _Currently in the examples_ Wrap the callback based code under an FRP
        layer
  * [ ] Library consistency
      - Containers return their concrete representation and their children's
        return value
      - `MonadUI` shouldn't wrap IO actions, but abstract commands:
        * `data UI = UI [UIControl]`
        * `class MonadUI m b where runUI :: UI -> m b`
        * `instance MonadUI IO CUIControl where runUI = toCUIControlIO`

- - -

The aim of `Graphics.LibUI.Types` and friends is to end-up with code that looks
like:
```haskell
runUILoop do
    menu "File" [ "Open"
                , "Save"
                , UIMenuItemQuit
                ]
    void $ window "libui Control Gallery" 640 300 True $
        void $ tabs $ do
            tab "Basic Controls" $ do
                hbox $
                    button "Button"
                    checkbox "Checkbox"
                label "This is a label. Right now, labels can only span one line."
                void $ group "Entries" $
                    form [ formItem "Entry" (entry "")
                         , formItem "Entry" (entry "")
                         , formItem "Search Entry" (searchEntry "")
                         ]
            tab "Basic Controls" $ hbox $ do
                void $ group "Numbers" (return ())
                void $ group "Lists" (return ())
            tab "Data Choosers" mempty
```

- - -

Tested on OSX, Fedora and Ubuntu 14.04. A Vagrantfile is available.

## License
This code is published under the **GPLv3** license
