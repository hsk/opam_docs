default: test test.opt run run.opt

install:
	opam install xml-light
uninstall: clean
	opam uninstall xml-light

test.opt: install test.ml
	ocamlfind ocamlopt -package xml-light xml-light.cmxa test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package xml-light xml-light.cma test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o *.csv test test.opt
