default: ex01 ex01.opt run run.opt

install:
	opam install pcre
uninstall: clean
	opam uninstall pcre

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package pcre pcre.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package pcre pcre.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.pcre ex01 ex01.opt .omakedb* *.omc *.run
