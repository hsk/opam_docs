# OCurl

OCaml libcurl バインディング

## インストール

	$ opam install ocurl

## 使い方

```
let _ =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let connection = new Curl.handle in
  connection#set_url "http://google.com/";
  connection#perform;
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ocurl
uninstall: clean
  opam uninstall ocurl

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package curl -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package curl -linkpkg ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.ocurl ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  ocurl
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))

```


## 参考URL

https://github.com/ygrek/ocurl
https://github.com/ygrek/ocurl/tree/master/examples
