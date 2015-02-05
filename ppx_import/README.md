# ppx_import

データ定義を読み込みます。

## インストール

	$ opam install ppx_import

## 使い方

ex01_data.ml

```
type a = A | B of string | C of a * a
type b = {
  key : string;
  value : string;
}
```

ex01.ml

```
type a = [%import: Ex01_data.a]
type b = [%import: Ex01_data.b]

let _ =
  let a = C(A, B "test") in
  match a with
  | C(_,B s) -> Printf.printf "%s\n" s
  | _ -> assert false
```

実行結果

```
test
```

ex02.ml

```
type a = [%import: Ex01_data.a] [@@deriving show]
type b = [%import: Ex01_data.b]

let _ =
  let a = C(A, B "test") in
  Printf.printf "%s\n" (show_a a)
```

ppx_deriving showと組み合わせる事も出来ます。

実行結果

```
test
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ppx_import
uninstall: clean
  opam uninstall ppx_import

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ppx_import ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package ppx_import ex01.ml -o ex01
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
  ppx_import
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/whitequark/ppx_import
