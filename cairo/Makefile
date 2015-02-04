default: test test.opt test_font.opt run run.opt

install:
	opam install cairo
uninstall: clean
	opam uninstall cairo

test_font.opt: install test_font.ml
	ocamlfind ocamlopt -package cairo test_font.ml -o test_font.opt -linkpkg
test.opt: install test.ml
	ocamlfind ocamlopt -package cairo cairo.cmxa test.ml -o test.opt -linkpkg
test: install test.ml
	ocamlfind ocamlc -package cairo cairo.cma test.ml -o test -linkpkg
run: test
	./test
run.opt: test.opt
	./test.opt
	./test_font.opt
clean:
	rm -f *.cm* *.o *.cairo test *.opt .omakedb* *.omc *.run *.png
