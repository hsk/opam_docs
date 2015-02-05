default: ex01 ex01.opt run run.opt

install:
	opam install ppx_monad
uninstall: clean
	opam uninstall ppx_monad

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package ppx_monad ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package ppx_monad ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o ex01 ex01.opt .omakedb* *.omc
