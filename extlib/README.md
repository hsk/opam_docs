# Extlib

便利な関数等

## インストール

	$ opam install extlib

## 使い方

```
let _ =
  Printf.printf "%s\n" (Std.dump 42);
  Printf.printf "%s\n" (String.concat "," 
    (List.map string_of_int (ExtList.List.unique [1;1;2])))
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
	opam install extlib
uninstall: clean
	opam uninstall extlib

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package extlib extlib.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package extlib extlib.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.extlib ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  extlib
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://code.google.com/p/ocaml-extlib/

http://ocaml-extlib.googlecode.com/svn/doc/apiref/index.html

http://ocaml.jp/%E5%AE%9A%E7%95%AA%E3%83%A9%E3%82%A4%E3%83%96%E3%83%A9%E3%83%AA
