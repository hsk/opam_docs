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
default: ex01 ex01.opt run run.opt

install:
  opam install ppx_deriving
uninstall: clean
  opam uninstall ppx_deriving

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ppx_deriving.show ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package ppx_deriving.show ex01.ml -o ex01
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
  ppx_deriving.show
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

[ppx_deriving.mli](~/.opam/system/lib/ppx_deriving/ppx_deriving.mli)

[ppx_deriving.mli](https://github.com/whitequark/ppx_deriving/blob/master/src/ppx_deriving.mli)