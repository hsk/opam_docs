default: test test.opt run

test.opt: test.ml
	ocamlfind ocamlopt graphics.cmxa test.ml -o test.opt
test: test.ml
	ocamlfind ocamlc graphics.cma test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt .omakedb* *.omc *.run
