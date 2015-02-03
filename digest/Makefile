default: test test.opt run run.opt

install:
uninstall: clean

test.opt: install test.ml
	ocamlfind ocamlopt test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt
