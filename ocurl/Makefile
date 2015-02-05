default: ex01 ex01.opt run run.opt

install:
	opam install ocurl
uninstall: clean
	opam uninstall ocurl

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package curl -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package curl -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.ocurl ex01 ex01.opt .omakedb* *.omc *.run
