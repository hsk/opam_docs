all: jasmc test install

jasmc: main.ml parser.mly lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -package javalib -linkpkg -o jasmc parser.ml lexer.ml main.ml
test:
	./jasmc tests/*.j

install:
	cp jasmc /usr/local/bin/

clean:
	rm jasmc *.cm* parser.ml lexer.ml parser.mli 
