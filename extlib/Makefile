default: ex01 ex01.opt run run.opt

install:
	opam install extlib
uninstall: clean
	opam uninstall extlib

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package extlib extlib.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package extlib extlib.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.extlib ex01 ex01.opt .omakedb* *.omc *.run
