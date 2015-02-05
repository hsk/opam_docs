default: ex01 ex01.opt run run.opt

install:
	opam install ocamlnet
uninstall: clean
	opam uninstall ocamlnet

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package netclient -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package netclient -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.ocamlnet ex01 ex01.opt .omakedb* *.omc *.run
