ghci:
	stack ghc --verbose -- --interactive -L./vendor/libui/build/out/ -lui -optl-Wl,-rpath,'$ORIGIN'
