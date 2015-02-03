default: test test.opt run run.opt

install:
	opam install csv
uninstall: clean
	opam uninstall csv

test.opt: install test.ml
	ocamlfind ocamlopt -package csv csv.cmxa test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package csv csv.cma test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o *.csv test test.opt .omakedb* *.omc *.run
