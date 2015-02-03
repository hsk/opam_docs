default: test test.opt run run.opt

install:
	opam install camlimages
uninstall: clean
	opam uninstall camlimages

test.opt: install test.ml
	ocamlfind ocamlopt -package camlimages.all_formats,camlimages.freetype,camlimages.graphics -linkpkg test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package camlimages.all_formats,camlimages.freetype,camlimages.graphics -linkpkg test.ml -o test

run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o *.camlimages test test.opt *.omc *.run .omakedb* *.png
