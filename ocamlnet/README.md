# OCamlNet

インターネットプロトコル（http、cgi、emailなど）およびヘルパーデータ構造（メールメッセージ、文字セットなど）

## インストール

	$ opam install ocamlnet

## 使い方

```
let _ =
  let s = Http_client.Convenience.http_get "http://www.caml.org/" in
  Printf.printf "%s\n" s
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ocamlnet
uninstall: clean
  opam uninstall ocamlnet

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package netclient -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package netclient -linkpkg ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.ocamlnet ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  netclient
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))

```


## 参考URL

http://ocamlnet.sourceforge.net/manual/refman/

http://ymotongpoo.hatenablog.com/entry/20100225/1267067912
