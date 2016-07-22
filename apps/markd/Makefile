markd: FORCE
	stack build --ghc-options=-O2 --ghc-options=-threaded
	cp .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/markd/markd .
	strip ./markd
	upx ./markd
	du -h markd

build: FORCE
	stack build

FORCE:
