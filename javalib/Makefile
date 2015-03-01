default: A.class ex01 ex01.opt ex5 run run.opt


install:
	opam install javalib
uninstall: clean
	opam uninstall javalib

A.class: A.java
	javac A.java

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package javalib -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package javalib -linkpkg ex01.ml -o ex01
ex5: ex5.ml
	ocamlfind ocamlc -package javalib -linkpkg ex5.ml -o ex5
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.csv ex01 ex01.opt .omakedb* *.omc *.run ex02 ex03 ex5 ex6
