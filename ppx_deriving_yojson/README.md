# PPX_deriving_yojson

データをJSONにマッピングします。

## インストール

	$ opam install ppx_deriving_yojson

## 使い方

```
type v =
  | A
  | B of int
  | C of int * string
[@@deriving yojson]

type vs = v list
[@@deriving yojson]

let _ =
  let json = [A;B 1; C(2,"aaa")] in
  Printf.printf "%s\n" (Yojson.Safe.to_string (vs_to_yojson json))


type geo = {
  lat [@key "Latitude"]  : float;
  lon [@key "Longitude"] : float;
}
[@@deriving yojson]

let _ =
  let geo = {lat=1.2; lon=5.55} in
  Printf.printf "%s\n" (Yojson.Safe.to_string (geo_to_yojson geo))
```

```
[["A"],["B",1],["C",2,"aaa"]]
{"Latitude":1.2,"Longitude":5.55}
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ppx_deriving_yojson
uninstall: clean
  opam uninstall ppx_deriving_yojson

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ppx_deriving_yojson ex01.ml -o ex01.opt -linkpkg
ex01: install ex01.ml
  ocamlfind ocamlc -package ppx_deriving_yojson ex01.ml -o ex01 -linkpkg
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
  ppx_deriving_yojson
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/whitequark/ppx_deriving_yojson
