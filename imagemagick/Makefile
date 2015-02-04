default: test test.opt run run.opt

install:
	opam install imagemagick
uninstall: clean
	opam uninstall imagemagick

test.opt: install test.ml
	ocamlfind ocamlopt -package magick test.ml -o test.opt -linkpkg
test: install test.ml
	ocamlfind ocamlc -package magick test.ml -o test -linkpkg
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o *.imagemagick test test.opt .omakedb* *.omc *.run
