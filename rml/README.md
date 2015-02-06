# rml

reactive ml

## インストール

	$ opam install rml

## 使い方

```
let process main = print_endline "Hello World!"
let () = run main
```


Makefile

```
default: ex01.opt run.opt

install:
  opam install rml
uninstall: clean
  opam uninstall rml

ex01.opt: install ex01.rml
  rmlc ex01.rml
  ocamlfind ocamlopt -o ex01.opt -I `rmlc -where` -package unix -linkpkg rmllib.cmxa ex01.ml

run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o ex01 ex01.opt .omakedb* *.omc *.run ex01.ml *.rzi
```

## 参考URL

http://rml.lri.fr/
