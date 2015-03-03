all: tests/HelloWeb.j jasmc test install

jasmc: main.ml parser.mly lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -package javalib -linkpkg -o jasmc parser.ml lexer.ml main.ml
test:
	./jasmc tests/*.j
	cat tests/HelloWeb.j
	./jasmc tests/HelloWeb.j

install:
	cp jasmc /usr/local/bin/

clean:
	rm jasmc *.cm* parser.ml lexer.ml parser.mli 

tests/HelloWeb.j:
	brew install jasmin
	wget http://sourceforge.net/projects/jasmin/files/jasmin/jasmin-2.4/jasmin-2.4.zip/download -O jasmin-2.4.zip
	unzip -o jasmin-2.4.zip
	mkdir tests
	cp jasmin-2.4/examples/* tests/.
	rm jasmin-2.4.zip
