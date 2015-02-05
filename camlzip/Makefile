default: ex01 ex01.opt minizip minigzip run run.opt

install:
	opam install camlzip
uninstall: clean
	opam uninstall camlzip

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package camlzip zip.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package camlzip zip.cma ex01.ml -o ex01


minizip: minizip.ml
	ocamlfind ocamlc -package camlzip -linkpkg minizip.ml -o minizip
minizip.ml:
	wget https://raw.githubusercontent.com/Leonidas-from-XIV/camlzip/master/test/minizip.ml

minigzip: minigzip.ml
	ocamlfind ocamlc -package camlzip -linkpkg minigzip.ml -o minigzip
minigzip.ml:
	wget https://raw.githubusercontent.com/Leonidas-from-XIV/camlzip/master/test/minigzip.ml

run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.camlzip ex01 ex01.opt .omakedb* *.omc *.run test* minigzip minizip minigzip.ml	minizip.ml
