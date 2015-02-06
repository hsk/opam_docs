# yojson

JSONの読み書き

## インストール

	$ opam install yojson

## 使い方

jsonのデータ構造は以下になります。

```
type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]
```

from_stringで文字列から、パースして結果を色々弄れます。

```
(* parse *)
let _ =

  let json = Yojson.Basic.from_string "{\"aaa\":\"bbb\"}" in
  match json with
  | `Assoc([k,`String v]) ->
    Printf.printf "%s %s\n" k v
  | _ -> assert false
```

from_string以外にもfrom_channel,from_file等があります。


to_stringで文字列に変換出来ます。

```
(* to_string *)
let _ =
  let json = `Assoc(["data",`String "v"]) in
  Printf.printf "%s\n" (Yojson.Basic.to_string json)
```

to_string以外にも、to_fileやto_outbuf等があります。


Utilを使うと色々便利です。

```
(* util *)
open Yojson.Basic.Util
let _ =
  let json = Yojson.Basic.from_string "{\"aaa\":\"bbb\",\"bbb\":\"ccc\"}" in
  Printf.printf "%s\n" (json |> member "aaa" |> to_string);
  Printf.printf "%s\n" (json |> member "bbb" |> to_string)
```

prettyプリントや、sort,filter等もあって便利です。

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install yojson
uninstall: clean
  opam uninstall yojson

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package yojson -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package yojson -linkpkg ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.yojson ex01 ex01.opt .omakedb* *.omc *.run
info:
  find `ocamlfind printconf path` | grep yojson | grep ".mli" | xargs cat 
  find `ocamlfind printconf path` | grep yojson | grep ".mli" 

```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  yojson
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

- https://github.com/mjambon/yojson
- http://mjambon.com/yojson-doc/Yojson.html
- http://mjambon.com/yojson-doc/Yojson.Basic.html
- http://mjambon.com/yojson-doc/Yojson.Safe.html
- http://mjambon.com/yojson-doc/Yojson.Raw.html
- http://mjambon.com/yojson-doc/Yojson_biniou.html
- [common.mli](https://github.com/mjambon/yojson/blob/master/common.mli)
- [pretty.mli](https://github.com/mjambon/yojson/blob/master/pretty.mli)
- [read.mli](https://github.com/mjambon/yojson/blob/master/read.mli)
- [safe.mli](https://github.com/mjambon/yojson/blob/master/safe.mli)
- [util.mli](https://github.com/mjambon/yojson/blob/master/util.mli)
- [write.mli](https://github.com/mjambon/yojson/blob/master/write.mli)
- [write2.mli](https://github.com/mjambon/yojson/blob/master/write2.mli)
- [yojson_biniou.mli](https://github.com/mjambon/yojson/blob/master/yojson_biniou.mli)
- [yojson.mli.cppo](https://github.com/mjambon/yojson/blob/master/yojson.mli.cppo)