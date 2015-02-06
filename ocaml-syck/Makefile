default: ex01 ex01.opt run run.opt

ocaml-syck:
	git clone https://github.com/mk270/ocaml-syck.git

install: ocaml-syck
	brew install syck
	opam install oasis
	cd ocaml-syck;oasis setup
	cd ocaml-syck;ocaml setup.ml -configure
	cd ocaml-syck;ocaml setup.ml -build
	cd ocaml-syck;ocaml setup.ml -install

uninstall:
	ocamlfind remove yaml

ex01.opt: ex01.ml
	ocamlfind ocamlopt -package yaml -linkpkg ex01.ml -o ex01.opt
ex01: ex01.ml
	ocamlfind ocamlc -package yaml -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -rf ocaml-syck *.cm* *.o *.csv ex01 ex01.opt .omakedb* *.omc *.run
