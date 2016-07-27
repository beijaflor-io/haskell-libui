# haskell-libui
- - -

Haskell bindings to the [`libui`](https://github.com/andlabs/libui) C library.

The library is currently only an FFI wrapper.

Useful top-level modules are:
- `Graphics.LibUI`
- `Graphics.LibUI.OSX`

Which export general and OSX specific functionality in raw C and "wrapped"
Haskell APIs.

- - -

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

Tested on OSX and Ubuntu 14.04. A Vagrantfile is available.
