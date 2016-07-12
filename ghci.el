"stack ghc -- --interactive -L./vendor/libui/build/out/ -lui -optl-Wl,-rpath,'$ORIGIN' src/Graphics/LibUI.hs"

(setq haskell-process-path-ghci '("stack"))
(setq haskell-process-args-ghci '("ghc" "--" "--interactive" "-L./vendor/libui/build/out/" "-lui" "-optl-Wl,-rpath,'$ORIGIN'"))
(setq haskell-process-type 'ghci)
