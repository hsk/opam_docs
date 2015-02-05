default: ex01 ex01.opt run run.opt

install:
	opam install ppx_deriving_yojson
uninstall: clean
	opam uninstall ppx_deriving_yojson

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package ppx_deriving_yojson ex01.ml -o ex01.opt -linkpkg
ex01: install ex01.ml
	ocamlfind ocamlc -package ppx_deriving_yojson ex01.ml -o ex01 -linkpkg
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o ex01 ex01.opt *.omc .omakedb*
