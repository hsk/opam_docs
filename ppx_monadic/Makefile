default: test test.opt run run.opt

install:
	opam install ppx_monadic
uninstall: clean
	opam uninstall ppx_monadic

test.opt: install test.ml
	ocamlfind ocamlopt -package ppx_monadic test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package ppx_monadic test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt
