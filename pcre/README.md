# PCRE

正規表現ライブラリ

## インストール

	$ opam install pcre

## 使い方

```
let _ =
  let s = Pcre.exec ~rex:(Pcre.regexp "aaa(.*)bbb") "aaaaaabbbccc" in
  let ss = Pcre.get_substrings s in
  let ss = Array.to_list ss in
  List.iter(fun(s) ->
    Printf.printf "%s\n" s
  ) ss
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install pcre
uninstall: clean
  opam uninstall pcre

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package pcre pcre.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package pcre pcre.cma ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.pcre ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  pcre
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/mmottl/pcre-ocaml

https://github.com/mmottl/pcre-ocaml/blob/master/lib/pcre.mli

http://takeisamemo.blogspot.jp/2014/05/ocamlpcre-ocaml.html

