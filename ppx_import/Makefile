default: ex01 ex01.opt ex02 run

install:
	opam install ppx_import
uninstall: clean
	opam uninstall ppx_import

ex01.opt: install ex01.ml ex01_data.ml
	ocamlfind ocamlopt -package ppx_import ex01_data.ml ex01.ml -o ex01.opt
ex01: install ex01.ml ex01_data.ml
	ocamlfind ocamlc -package ppx_import ex01_data.ml ex01.ml -o ex01

ex02: install ex02.ml ex01_data.ml
	ocamlfind ocamlc -package ppx_import,ppx_deriving.show ex01_data.ml ex02.ml -o ex02

run: ex02 ex01
	./ex01
	./ex02
clean:
	rm -f *.cm* *.o ex01 ex02 ex01.opt *.omc .omakedb*
