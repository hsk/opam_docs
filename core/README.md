# Core

Jane Street で開発されている基本ライブラリで、Realworld OCamlでも基本的なライブラリとして使われています。

## インストール

	$ opam install core

realworldの本によると、以下の方が良いのかもしれません。

	$ opam install core core_extended core_bench async

## 使い方

```
open Core.Std

let _ =
  let list = [1;2;3] in
  let rc = List.fold_left list ~init:0 ~f:(fun acc x ->
    acc + x
  ) in
  Printf.printf "%d\n" rc
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
	opam install core
uninstall: clean
	opam uninstall core

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -thread -package core -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -thread -package core -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.core ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  core
OCAMLCFLAGS += -thread
OCAMLOPTFLAGS += -thread

.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

http://d.hatena.ne.jp/camlspotter/20090906/1252235911

https://github.com/realworldocaml/examples

https://ocaml.janestreet.com/ocaml-core/111.28.00/doc/core/

https://github.com/realworldocaml/examples/tree/master/code/installation

