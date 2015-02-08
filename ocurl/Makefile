default: ex01.opt run run2

install:
	opam install ocurl
uninstall: clean
	opam uninstall ocurl

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package curl -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package curl -linkpkg ex01.ml -o ex01
ex02: install ex02.ml
	ocamlfind ocamlc -package curl -linkpkg ex02.ml -o ex02
run: ex01
	./ex01
run2: ex02
	./ex02
clean:
	rm -f *.cm* *.o *.ocurl ex01 ex02 ex01.opt .omakedb* *.omc *.run
