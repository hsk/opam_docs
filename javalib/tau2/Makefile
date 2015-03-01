all: AbsTau.ml main.ml
	ocamlyacc ParTau.mly
	ocamllex LexTau.mll
	ocamlfind ocamlc -o TestTau -package javalib -linkpkg BNFC_Util.ml AbsTau.ml SkelTau.ml ShowTau.ml PrintTau.ml ParTau.mli ParTau.ml LexTau.ml main.ml
	./TestTau fact.tau
	java Tau

AbsTau.ml: Tau.cf
	bnfc -ocaml Tau.cf

clean:
	-rm -f *.cmi *.cmo *.o *.class

distclean: clean
	-rm -f LexTau.* ParTau.* LayoutTau.* SkelTau.* PrintTau.* ShowTau.* TestTau.* AbsTau.* TestTau BNFC_Util.ml
