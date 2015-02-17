default: run

install:

uninstall:

run: ex01 ex02 ex03
	./ex01
	./ex02
	./ex03

run.opt: ex01.opt
	./ex01.opt

ex01: ex01.ml
	ocamlc ex01.ml -o ex01
ex02: ex02.ml
	ocamlc ex02.ml -o ex02
ex03: ex03.ml
	ocamlc ex03.ml -o ex03
ex01.opt: ex01.ml
	ocamlopt ex01.ml -o ex01.opt

clean:
	rm -f *.cm* *.o ex01 ex01.opt ex02 ex03
