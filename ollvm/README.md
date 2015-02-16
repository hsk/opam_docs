# OLLVM

PureなOCamlで書かれたLLVM IRライブラリ

## インストール

	$ opam install ollvm

## 使い方

```
open Ollvm.Ez.Value
open Ollvm.Ez.Instr
open Ollvm.Ez.Block
module M = Ollvm.Ez.Module
module T = Ollvm.Ez.Type
module P = Ollvm.Printer

let _ =
  (* module initialization *)
  let m = M.init
            "name"
            ("x86_64", "pc", "linux-gnu")
            "e-m:e-i64:64-f80:128-n8:16:32:64-S128" in

  (* variables declaration *)
  let (m, x0) =
    M.local m T.i1 "" in
  let (m, [x1; x2; x3; arg]) =
    M.locals m T.i32 [""; ""; ""; ""] in
  let (m, [entry_b; then_b; else_b]) =
    M.locals m T.label ["entry"; "then"; "else" ] in
  let (m, fact) = M.global m T.i32 "fact" in
  let (m, x4) =
    M.local m T.i1 "" in

  (* fact function definition *)
  let f = define fact [x4]
                 [ block entry_b [
                           x0 <-- eq x4 (i32 0) ;
                           br x0 then_b else_b ; ] ;
                   block then_b [
                           ret (i32 1) ; ] ;
                   block else_b [
                           x1 <-- sub x4 (i32 1) ;
                           x2 <-- call fact [x1] ;
                           x3 <-- mul x4  x2 ;
                           ret x3 ; ] ] in

  (* fact function registration in module *)
  let m = M.definition m f in
  P.modul (P.empty_env ()) Format.std_formatter m.m_module
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ollvm
uninstall: clean
  opam uninstall ollvm

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ollvm ollvm.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package ollvm ollvm.cma ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.ollvm ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  ollvm
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL


