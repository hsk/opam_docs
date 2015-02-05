# ppx_blob

文字列を埋め込みます。

## インストール

	$ opam install ppx_blob

## 使い方

```
print_string [%blob "ex01.ml"]
```

実行結果

```
print_string [%blob "ex01.ml"]
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ppx_blob
uninstall: clean
  opam uninstall ppx_blob

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ppx_blob ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package ppx_blob ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o ex01 ex01.opt *.omc .omakedb*
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  ppx_blob
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/whitequark/ppx_import
