default: ex01 ex01.opt run run.opt

install:
	opam install yojson
uninstall: clean
	opam uninstall yojson

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package yojson -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package yojson -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.yojson ex01 ex01.opt .omakedb* *.omc *.run
info:
	find `ocamlfind printconf path` | grep yojson | grep ".mli" | xargs cat 
	find `ocamlfind printconf path` | grep yojson | grep ".mli" 

