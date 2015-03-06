all: examples/HelloWeb.j jasmc install test

jasmc: main.ml parser.mly lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -package javalib,ppx_deriving.show -linkpkg -o jasmc parser.ml lexer.ml main.ml
test: example
	@echo "--------"
	./jasmc tests/jsr.j
	javac -source 1.5 -target 1.5 tests/Test_while.java
	jasmd -high tests.Test_while
	jasmd tests.Test_while > tests/Test_while.j
	@echo "--------"
	cat tests/Test_while.j
	@echo "--------"
	javap -v tests/Test_while.class
	./jasmc tests/Test_while.j
	java tests.Test_while

example:
	./jasmc examples/*.j
	java examples.ANewArray
#	java examples.AnInterface
	java examples.Arrays a
#	java examples.Catch
	java examples.Checkcast
	java examples.Count
#	java examples.HelloWorld # error ok
	java examples.Implementor
#	java examples.InvokeInterface
	java examples.MultiANewArray
	java examples.NewArray
	java examples.Switch
#	java examples.Uncaught
#	java examples.VerifyTest # error ok
#	java examples.VerifyTest1 # error ok

install:
	cp jasmc /usr/local/bin/

clean:
	rm jasmc *.cm* parser.ml lexer.ml parser.mli examples/*.class tests/*.class examples/*.txt tests/*.txt 

distclean:
	rm -rf examples

examples/HelloWeb.j:
	brew install jasmin
	wget http://sourceforge.net/projects/jasmin/files/jasmin/jasmin-2.4/jasmin-2.4.zip/download -O jasmin-2.4.zip
	unzip -o jasmin-2.4.zip
	mkdir examples
	cp jasmin-2.4/examples/* examples/.
	rm jasmin-2.4.zip
