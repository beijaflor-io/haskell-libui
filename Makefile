build: FORCE
	stack build --install-ghc --allow-different-user

run: FORCE
	stack run -i

test: FORCE
	stack test

provision-ubuntu: FORCE
	bash -e provision.sh

ghci: FORCE
	stack ghc --verbose -- --interactive -L./vendor/libui/build/out/ -lui -optl-Wl,-rpath,'$ORIGIN'

FORCE:
