default: test test.opt test_font.opt run run.opt

install:
	opam install cairo2
uninstall: clean
	opam uninstall cairo2

test_font.opt: install test_font.ml
	ocamlfind ocamlopt -package cairo2 test_font.ml -o test_font.opt -linkpkg
test.opt: install test.ml
	ocamlfind ocamlopt -package cairo2 cairo2.cmxa test.ml -o test.opt -linkpkg
test: install test.ml
	ocamlfind ocamlc -package cairo2 cairo2.cma test.ml -o test -linkpkg
run: test
	./test
run.opt: test.opt
	./test.opt
	./test_font.opt
clean:
	rm -f *.cm* *.o *.cairo2 test *.opt .omakedb* *.omc *.run *.png
