all: run

install:
	opam install cow
uninstall:
	opam uninstall cow

clean:
	rm -rf ocaml-cow

run: ocaml-cow/tests/Makefile
	cd ocaml-cow && make tests
ocaml-cow/tests/Makefile:
	git clone https://github.com/mirage/ocaml-cow.git
