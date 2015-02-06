default: ex01 ex01.opt ex02 run

install:
	opam install ocamlnet
uninstall: clean
	opam uninstall ocamlnet

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package netclient -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package netclient -linkpkg ex01.ml -o ex01
ex02: install ex02.ml
	ocamlfind ocamlc -package netclient,netstring -linkpkg ex02.ml -o ex02
run: ex01 ex02
	./ex02
clean:
	rm -f *.cm* *.o *.ocamlnet ex01 ex01.opt ex02 .omakedb* *.omc *.run
