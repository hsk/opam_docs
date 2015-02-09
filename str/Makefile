default: ex01 ex01.opt run run.opt

install:
uninstall: clean

ex01.opt: install ex01.ml
	ocamlfind ocamlopt str.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc str.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o ex01 ex01.opt
