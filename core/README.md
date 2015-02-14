# Core

ジェットな、つおいライブラリです。

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
default: ex01.native run

install:
	opam install core
uninstall: clean
	opam uninstall core

ex01.native: install ex01.ml
	ocamlbuild -use-ocamlfind -package core -tag thread ex01.native
run: ex01.native
	./ex01.native
clean:
	rm -f *.cm* *.o *.core ex01.native .omakedb* *.omc _build
```

## 参考URL

http://d.hatena.ne.jp/camlspotter/20090906/1252235911

https://github.com/realworldocaml/examples

https://ocaml.janestreet.com/ocaml-core/111.28.00/doc/core/

https://github.com/realworldocaml/examples/tree/master/code/installation

