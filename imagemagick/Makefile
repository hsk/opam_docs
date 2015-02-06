default: ex01 ex01.opt run run.opt

install:
	opam install imagemagick
uninstall: clean
	opam uninstall imagemagick

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package magick ex01.ml -o ex01.opt -linkpkg
ex01: install ex01.ml
	ocamlfind ocamlc -package magick ex01.ml -o ex01 -linkpkg
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.imagemagick ex01 ex01.opt .omakedb* *.omc *.run *.jpg
