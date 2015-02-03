default: test test.opt run run.opt

install:
	opam install base64
uninstall: clean
	opam uninstall base64

test.opt: install test.ml
	ocamlfind ocamlopt -package base64 base64.cmxa test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package base64 base64.cma test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt
