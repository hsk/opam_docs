# Planck

Planckはモナディックなパーサコンビネータライブラリです。

## インストール

	$ opam install planck

## 使い方

```
type e = 
  | Const of int
  | Binop of char * e * e
  | Unop of char * e

module ExpParser = struct

  open Planck

  module Stream = Schar
  module Base = Pbase.Make(Stream)
  include Base
  include Pbuffer.Extend(Stream)(Base)

  module Op = Op_prec.Make(struct
    type t = e
    type op = char
    let show_op = Printf.sprintf "(%c)"
    let app _f _a      = assert false
    let binop op a1 a2 = Binop(op, a1, a2)
    let unop op a1     = Unop(op, a1)
  end)

  let tbl = 
    let open Op_prec.Operator in
    [
      '+',  { prec = 2.0; kind = `Infix `Left };
      '-',  { prec = 2.0; kind = `Infix `Left };
      '*',  { prec = 3.0; kind = `Infix `Left };
      '/',  { prec = 3.0; kind = `Infix `Left };
      '~',  { prec = 5.0; kind = `Prefix };
    ]

  let blank = void (one_of [' '; '\t'; '\n'; '\r'])

  let rec parse s =
    let stream = Stream.from_string ~filename:"stdin" s in
    expr stream

  and simple_expr st = st |> (
    
    ?* blank >>= fun () -> (

    constant

    <|> (tokenp (function '+' | '-' | '*' | '/' | '~' -> true | _ -> false)
           >>= fun char -> return (`Op (List.assoc char tbl, char)))

    <|> (token '(' >>= fun () ->
         expr >>= fun e ->
         ?* blank >>= fun () ->
         token ')' >>= fun () -> 
         return (`Term e))
    )
  )
  and constant st = st |> (

    matched (?+ (tokenp (function '0'..'9' -> true | _ -> false) <?> "decimal")) 
    >>= fun s -> return (`Term (Const (int_of_string s)))
  )
  and expr st = st |> (
    option (token '-') >>= fun unary_minus ->
    ?++ simple_expr >>= fun es -> 
    match unary_minus with
    | Some () -> return (Op.parse (`Op (List.assoc '~' tbl, '~') :: es))
    | None -> return (Op.parse es) 
  )

end

let rec eval = function
  | Const n -> n
  | Binop ('+', t1, t2) -> eval t1 + eval t2
  | Binop ('-', t1, t2) -> eval t1 - eval t2
  | Binop ('*', t1, t2) -> eval t1 * eval t2
  | Binop ('/', t1, t2) -> eval t1 / eval t2
  | Unop ('~', t1) -> - eval t1
  | _ -> assert false

let _ = 
  match ExpParser.parse "1 + 2 * 3" with
  | `Ok (res, _) -> Format.printf "%d@." (eval res);
  | `Error (pos, s) ->
    Format.eprintf "%a: syntax error: %s@." Planck.Position.File.format pos s
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install planck
uninstall: clean
  opam uninstall planck

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package planck -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package planck -linkpkg ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.planck ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  planck
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://bitbucket.org/camlspotter/planck

- https://bitbucket.org/camlspotter/planck/src/tip/lib/
- https://bitbucket.org/camlspotter/planck/src/tip/lib/elem.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/op_prec.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pbase.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pbufchar.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pbuffer.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pchar.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pfile.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pmemo.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/position.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/profile.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/pstream.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/ptoken.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/sbuffer.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/schar.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/sfile.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/smemo.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/sstring.mli
- https://bitbucket.org/camlspotter/planck/src/tip/lib/stoken.mli
