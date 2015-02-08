# mParser

シンプルなモナディックパーサコンビネータライブラリ

## インストール

	$ opam install mparser

## 使い方

```
open MParser

let infix p o =
  Infix (p |>> (fun _ a b -> (`Binop (o, a, b))), Assoc_left)

let operators =
  [ [ infix (char '*') `Mul;
      infix (char '/') `Div ];
    [ infix (char '+') `Add;
      infix (char '-') `Sub ] ]

let expr =
  expression operators (Tokens.decimal |>> fun i -> `Int i)

let rec calc = function
  | `Int i -> i
  | `Binop (op, a, b) ->
      match op with
        | `Add -> calc a + calc b
        | `Sub -> calc a - calc b
        | `Mul -> calc a * calc b
        | `Div -> calc a / calc b

let eval (s: string) : int =
  match MParser.parse_string expr s () with
    | Success e -> calc e
    | Failed (msg, e) -> failwith msg

let _ =
  Printf.printf "%d\n" (eval "4 *4+10/2")
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
	opam install mparser
uninstall: clean
	opam uninstall mparser

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package mparser -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package mparser -linkpkg ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.mparser ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  mparser
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://bitbucket.org/cakeplus/mparser
