default: ex01 ex01.opt run run.opt

install:
	opam install core
uninstall: clean
	opam uninstall core

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -thread -package core -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -thread -package core -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.core ex01 ex01.opt .omakedb* *.omc *.run
