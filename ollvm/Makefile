default: ex01 ex01.opt ex02 run

install:
	opam install ollvm
uninstall: clean
	opam uninstall ollvm

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package ollvm ollvm.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package ollvm ollvm.cma ex01.ml -o ex01

ex02: install ex02.ml
	ocamlfind ocamlc -package ollvm -linkpkg ex02.ml -o ex02
run: ex01 ex02
	./ex01
	./ex02
clean:
	rm -f *.cm* *.o *.ollvm ex01 ex02 *.opt .omakedb* *.omc *.run
