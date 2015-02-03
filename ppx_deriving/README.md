# PPX_deriving

型に自動的に文字列化等の機能を追加します。

## インストール

	$ opam install ppx_deriving

## 使い方

```
type e =
  | Int of int
  | Add of e * e
[@@deriving show]

let _ =
  let e = Add(Int 1, Int 2) in
  Printf.printf "e=%s\n" (show_e e);
  Format.fprintf Format.std_formatter "e=%a\n" pp_e e;;
```

Makefile

```
default: test test.opt run run.opt

install:
  opam install ppx_deriving
uninstall: clean
  opam uninstall ppx_deriving

test.opt: install test.ml
  ocamlfind ocamlopt -package ppx_deriving.show test.ml -o test.opt
test: install test.ml
  ocamlfind ocamlc -package ppx_deriving.show test.ml -o test
run: test
  ./test
run.opt: test.opt
  ./test.opt
clean:
  rm -f *.cm* *.o test test.opt
```
