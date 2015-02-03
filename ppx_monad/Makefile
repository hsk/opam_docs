default: test test.opt run run.opt

install:
	opam install ppx_monad
uninstall: clean
	opam uninstall ppx_monad

test.opt: install test.ml
	ocamlfind ocamlopt -package ppx_monad test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package ppx_monad test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt
