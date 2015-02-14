default: ex01.native run

install:
	opam install core
uninstall: clean
	opam uninstall core

ex01.native: install ex01.ml
	ocamlbuild -use-ocamlfind -package core -tag thread ex01.native
run: ex01.native
	./ex01.native
clean:
	rm -rf *.cm* *.o *.core ex01.native .omakedb* *.omc _build
