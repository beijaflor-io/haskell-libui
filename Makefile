build: FORCE
	git submodule update --init
	stack build --install-ghc --allow-different-user

vagrant-build: FORCE
	vagrant up && vagrant provision && vagrant ssh -c 'cd /vagrant && make build'

libui: FORCE
	cd ./vendor/libui && mkdir -p build && cd build && rm -rf * && cmake .. && make examples

run: FORCE
	stack run -i

test: FORCE
	stack test

provision-ubuntu: FORCE
	bash -e provision.sh

ghci: FORCE
	stack ghc --verbose -- --interactive -L./vendor/libui/build/out/ -lui -optl-Wl,-rpath,'$ORIGIN'

FORCE:
