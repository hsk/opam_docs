default: ex01 ex01.opt run run.opt

install:
	opam install ppx_deriving
uninstall: clean
	opam uninstall ppx_deriving

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package ppx_deriving.show ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package ppx_deriving.show ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o ex01 ex01.opt *.omc .omakedb*
