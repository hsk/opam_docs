default: ex01 ex01.opt run run.opt

install:
	opam install csv
uninstall: clean
	opam uninstall csv

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package csv csv.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package csv csv.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.csv ex01 ex01.opt .omakedb* *.omc *.run
