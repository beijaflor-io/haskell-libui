build:
	stack build --install-ghc

provision-ubuntu:
	bash -e provision.sh

ghci:
	stack ghc --verbose -- --interactive -L./vendor/libui/build/out/ -lui -optl-Wl,-rpath,'$ORIGIN'
