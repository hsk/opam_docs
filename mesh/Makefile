default: ex01 ex01.opt run run.opt

install:
	opam install mesh
uninstall: clean
	opam uninstall mesh

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package mesh mesh.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package mesh mesh.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.mesh ex01 ex01.opt .omakedb* *.omc *.run

info:
	find `ocamlfind printconf path` | grep mesh | grep ".mli" | xargs cat 
	find `ocamlfind printconf path` | grep mesh | grep ".mli" 

