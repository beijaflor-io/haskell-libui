build: FORCE
	git submodule update --init
	stack build --install-ghc --allow-different-user

READMEExample: FORCE
	stack ghc --package libui ./READMEExample.hs
	rm READMEExample.{o,hi}

vagrant-build: FORCE
	vagrant up && vagrant provision && vagrant ssh -c 'cd /vagrant && make build'

markd: FORCE
	cd ./apps/markd/ && make

gists: FORCE
	cd ./apps/gists/ && make

libui: FORCE
	cd ./vendor/libui && mkdir -p build && cd build && rm -rf * && cmake .. && make examples

run: FORCE
	stack run -i

test: FORCE
	stack test

provision-ubuntu: FORCE
	bash -e provision.sh

./vendor/libui/build/out/libui.so: FORCE
	cd ./vendor/libui && rm -rf build && mkdir build && cd build && cmake .. && make

ghci: FORCE
	make ./vendor/libui/build/out/libui.so
	stack ghc --verbose -- --interactive -L./vendor/libui/build/out/ -lui -optl-Wl,-rpath,'$ORIGIN'

ghci-linux: FORCE
	stack ghc -- --interactive -L./vendor/libui/build/out/ -lcairo -lui

FORCE:
