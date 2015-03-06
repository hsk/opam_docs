all: examples/HelloWeb.j jasmc install

jasmc: main.ml parser.mly lexer.mll
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlfind ocamlc -package javalib,ppx_deriving.show -linkpkg -o jasmc parser.ml lexer.ml main.ml
test: example
	./jasmc tests/Test_switch.j
	java tests.Test_switch
	./jasmc tests/Test_switch2.j
	java tests.Test_switch2
	./jasmc tests/Test_switch3.j
	java tests.Test_switch3

	@echo "--------"
	./jasmc tests/jsr.j
	javac -source 1.5 -target 1.5 tests/*.java
	jasmd -high tests.Test_while
	jasmd tests.Test_while > tests/Test_while.j
	@echo "--------"
	cat tests/Test_while.j
	@echo "--------"
	javap -v tests/Test_while.class
	./jasmc tests/Test_while.j
	java tests.Test_while


#	./jasmc tests/InvokeDynamic.j
	./jasmc tests/Test001.j
	./jasmc tests/Test002.j
	./jasmc tests/Test003.j
	./jasmc tests/Test004.j
	./jasmc tests/Test005.j
	./jasmc tests/Test006.j
	./jasmc tests/Test007.j
	./jasmc tests/Test008.j
	./jasmc tests/Test009.j
	./jasmc tests/Test010.j
	./jasmc tests/Test_b.j
	./jasmc tests/Test_c.j
	./jasmc tests/Test_d.j
	./jasmc tests/Test_f.j
	./jasmc tests/Test_g.j
	./jasmc tests/Test_i.j
	./jasmc tests/Test_interface.j
	./jasmc tests/Test_interface2.j
	./jasmc tests/Test_l.j
	./jasmc tests/Test_m.j
	./jasmc tests/Test_s.j

#	java tests.InvokeDynamic
#	java tests.Test001
#	java tests.Test002
	java tests.Test003
	java tests.Test004
	java tests.Test005
	java tests.Test006 aaa
	java tests.Test007
	java tests.Test008
	java tests.Test009
	java tests.Test010
	java tests.Test_b
	java tests.Test_c
	java tests.Test_d
	java tests.Test_f
	java tests.Test_g
	java tests.Test_i
#	java tests.Test_interface
	java tests.Test_interface2
	java tests.Test_l
	java tests.Test_m
	java tests.Test_s
	./jasmc tests/Test_throws.j
#	java tests.Test_throws
	./jasmc tests/Test_while.j
	java tests.Test_while
	./jasmc tests/jsr.j
	java tests.jsr
	./jasmc tests/jsr2.j
	java tests.jsr2

example:
	./jasmc examples/*.j
	java examples.ANewArray
#	java examples.AnInterface
	java examples.Arrays a
	java examples.Catch
	java examples.Checkcast
	java examples.Count
#	java examples.HelloWorld # error ok
	java examples.Implementor
	java examples.InvokeInterface
	java examples.MultiANewArray
	java examples.NewArray
	java examples.Switch
#	java examples.Uncaught # exception ok
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
