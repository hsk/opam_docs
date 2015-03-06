all: jasmd1
install: jasmd
	cp jasmd /usr/local/bin/.
uninstall:
	rm /usr/local/bin/jasmd

jasmd1: 
	ocamlfind ocamlc -o jasmd \
		-package javalib -linkpkg \
		pJBasics.ml pJSignature.ml pJCode.ml pJClass.ml pJClassLow.ml main.ml

test:
	javac A.java
	./jasmd A
clean:
	rm -rf jasmd pJClassLow *.cm* javalib-2.3/src/*.cm* *.o *.class
