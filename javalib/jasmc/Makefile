all: examples/HelloWeb.j jasmc test install

jasmc: main.ml parser.mly lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -package javalib,ppx_deriving.show -linkpkg -o jasmc parser.ml lexer.ml main.ml
test: example
	@echo "--------"
	./jasmc tests/jsr.j
	javac -source 1.5 -target 1.5 tests/Test_switch.java
	jasmd -high tests.Test_switch
	jasmd tests.Test_switch > tests/Test_switch.j
	@echo "--------"
	cat tests/Test_switch.j
	@echo "--------"
	cp tests/Test_switch.class tests/Test_switch.txt
	javap -v tests/Test_switch.class
	java tests.Test_switch
	./jasmc tests/Test_switch.j
	cp tests/Test_switch.class tests/Test_switch2.txt
	java tests.Test_switch
	./jasmc tests/Test_switch2.j

example:
	./jasmc examples/*.j
	java examples.ANewArray
#	java examples.AnInterface
#	java examples.Arrays
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
