# OLLVM

PureなOCamlで書かれたLLVM IRライブラリ

## インストール

	$ opam install ollvm

## 使い方

データ作成と出力

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

test.ll

```
; ModuleID = 'name'
target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"


define i32 @fact(i1 %0) {
entry:
        %1 = icmp eq i1 %0, 0
        br i1 %1, label %then, label %else
then:
       ret i32 1
else:
       %2 = sub i1 %0, 1
       %3 = call i32 @fact(i32 %2)
       %4 = mul i1 %0, %3
       ret i32 %4
}
```

パース

```
let parse input =
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Ollvm_lexer.parse lexbuf in
  close_in inp;
  ast

let _ =
  let ast = parse "test.ll" in
  Ollvm_printer.toplevelentries (Ollvm_printer.empty_env ()) Format.std_formatter ast
```

Makefile

```
default: ex01 ex01.opt ex02 run

install:
  opam install ollvm
uninstall: clean
  opam uninstall ollvm

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ollvm ollvm.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package ollvm ollvm.cma ex01.ml -o ex01

ex02: install ex02.ml
  ocamlfind ocamlc -package ollvm -linkpkg ex02.ml -o ex02
run: ex01 ex02
  ./ex01
  ./ex02
clean:
  rm -f *.cm* *.o *.ollvm ex01 ex02 *.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean ex02
USE_OCAMLFIND = true
FILES[]= ex01
FILES2[]= ex02
OCAMLPACKS[]=
  ollvm
OCAMLFLAGS[] += -w Aer-8-40-42$(if $(OCAML_ACCEPTS_Z_WARNING), z)
.DEFAULT: $(OCamlProgram ex01, $(FILES)) ex02
ex02: $(OCamlProgram ex02, $(FILES2))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL


https://github.com/OCamlPro/ollvm
