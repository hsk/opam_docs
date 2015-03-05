all: distinst jasmd1
install: jasmd
	cp jasmd /usr/local/bin/.
uninstall:
	rm /usr/local/bin/jasmd

jasmd1: 
# pJBasics.ml pJSignature.ml pJCode.ml pJClass.ml main.ml
	ocamlfind ocamlc -o jasmd \
		-package javalib -linkpkg \
		pJBasics.ml pJSignature.ml pJCode.ml pJClass.ml pJClassLow.ml main.ml

distinst: javalib-2.3/ptrees/ptmap.ml

distclean: clean
	rm -rf javalib-2.3*

test:
	javac A.java
	./jasmd A
clean:
	rm -rf jasmd pJClassLow *.cm* javalib-2.3/src/*.cm* *.o *.class
