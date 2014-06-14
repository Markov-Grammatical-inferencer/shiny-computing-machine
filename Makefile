all: dep
	cd src;	make all

test:
	cd src;make test

clean:
	cd src;make clean

dep:

# Commented out because the commands here don't seem to work properly
#dep: ocaml-curses/dllcurses_stubs.so
#
#ocaml-curses/dllcurses_stubs.so:
#	git submodule update
#	cd ocaml-curses
#	autoreconf
#	./configure.sh
#	make all opt
