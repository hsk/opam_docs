all: examples/HelloWeb.j jasmc test install

jasmc: main.ml parser.mly lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -package javalib -linkpkg -o jasmc parser.ml lexer.ml main.ml
test:
	@echo "--------"
	./jasmc examples/*.j
	javac tests/Test_g.java
	jasmd -high tests.Test_g
	jasmd tests.Test_g > tests/Test_g.j
	@echo "--------"
	cat tests/Test_g.j
	@echo "--------"
	./jasmc tests/Test_g.j


install:
	cp jasmc /usr/local/bin/

clean:
	rm jasmc *.cm* parser.ml lexer.ml parser.mli 

distclean:
	rm -rf examples

examples/HelloWeb.j:
	brew install jasmin
	wget http://sourceforge.net/projects/jasmin/files/jasmin/jasmin-2.4/jasmin-2.4.zip/download -O jasmin-2.4.zip
	unzip -o jasmin-2.4.zip
	mkdir examples
	cp jasmin-2.4/examples/* examples/.
	rm jasmin-2.4.zip
