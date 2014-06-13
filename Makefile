all: dep
	cd src;	make all

test:
	cd src;make test

clean:
	cd src;make clean

dep: ocaml-curses/dllcurses_stubs.so
	git submodule update;
	cd ocaml-curses;autoreconf;./configure.sh;make all opt
