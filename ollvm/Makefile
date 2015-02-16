default: ex01 ex01.opt run run.opt

install:
	opam install ollvm
uninstall: clean
	opam uninstall ollvm

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package ollvm ollvm.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package ollvm ollvm.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.ollvm ex01 ex01.opt .omakedb* *.omc *.run
