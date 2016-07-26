gists: FORCE
	stack build --ghc-options=-O2 --ghc-options=-threaded
	cp .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/gists/gists .
	strip ./gists
	upx ./gists
	du -h gists

build: FORCE
	stack build

FORCE:
