all: distinst jasmd
install: jasmd
	cp jasmd /usr/local/bin/.
jasmd: pJBasics.ml pJSignature.ml pJCode.ml pJClass.ml main.ml
	ocamlfind ocamlc -o jasmd -I javalib-2.3/src -I javalib-2.3/ptrees \
		-package camlp4 -syntax camlp4o \
		-package camlzip,extlib -linkpkg \
		Str.cma \
		javalib-2.3/ptrees/ptmap.mli \
		javalib-2.3/ptrees/ptmap.ml \
		javalib-2.3/ptrees/*.mli \
		javalib-2.3/ptrees/*.ml \
		javalib-2.3/src/JBasics.mli \
		javalib-2.3/src/JBasics.ml \
		javalib-2.3/src/JSignature.mli \
		javalib-2.3/src/JSignature.ml \
		javalib-2.3/src/JCode.mli \
		javalib-2.3/src/JCode.ml \
		javalib-2.3/src/JUnparseSignature.mli \
		javalib-2.3/src/JUnparseSignature.ml \
		javalib-2.3/src/JDumpBasics.mli \
		javalib-2.3/src/JDumpBasics.ml \
		javalib-2.3/src/JParseSignature.mli \
		javalib-2.3/src/JParseSignature.ml \
		javalib-2.3/src/JBasicsLow.mli \
		javalib-2.3/src/JBasicsLow.ml \
		javalib-2.3/src/JClassLow.mli \
		javalib-2.3/src/JParseCode.mli \
		javalib-2.3/src/JParseCode.ml \
		javalib-2.3/src/JInstruction.mli \
		javalib-2.3/src/JInstruction.ml \
		javalib-2.3/src/JClass.mli \
		javalib-2.3/src/JClass.ml \
		javalib-2.3/src/JHigh2Low.mli \
		javalib-2.3/src/JHigh2Low.ml \
		javalib-2.3/src/JUnparse.mli \
		javalib-2.3/src/JUnparse.ml \
		javalib-2.3/src/JLow2High.mli \
		javalib-2.3/src/JLow2High.ml \
		javalib-2.3/src/JDumpJasmin.mli \
		javalib-2.3/src/JDumpJasmin.ml \
		javalib-2.3/src/JPrint.ml \
		javalib-2.3/src/JParse.mli \
		javalib-2.3/src/JParse.ml \
		javalib-2.3/src/JFile.mli \
		javalib-2.3/src/JFile.ml \
		javalib-2.3/src/JDumpLow.mli \
		javalib-2.3/src/JDumpLow.ml \
		pJBasics.ml pJSignature.ml pJCode.ml pJClass.ml pJClassLow.ml main.ml

distinst: javalib-2.3/ptrees/ptmap.ml

distclean: clean
	rm -rf javalib-2.3*

javalib-2.3/ptrees/ptmap.ml:
	wget https://gforge.inria.fr/frs/download.php/33090/javalib-2.3.tar.bz2
	tar xvfz javalib-2.3.tar.bz2
test:
	javac A.java
	./jasmd A
clean:
	rm -rf jasmd pJClassLow *.cm* javalib-2.3/src/*.cm* *.o *.class
