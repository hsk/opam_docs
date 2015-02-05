# PPX_monad

モナド用の構文を追加します。

## インストール

	$ opam install ppx_monad

## 使い方

begin

```
(* begin *)
begin%monad
  x <- [1; 2; 3];
  y <- [3; 4; 5];
  return (x + y)
end
```
は以下のように変換されます:

```
let _ = [1; 2; 3] >>= (fun x  -> [3; 4; 5] >>= (fun y  -> return (x + y)))
```

fun

```
let f = fun%monad xs ys ->
  x <- xs;
  y <- ys;
  let z = x + y in
  return z
```

は以下のように変換されます:

```
let f xs ys = xs >>= (fun x  -> ys >>= (fun y  -> let z = x + y in return z))
```

function

```
let rec fibm = function%monad
  | 0 -> return 0
  | 1 -> return 1
  | n ->
    x <- fibm (n - 2);
    y <- fibm (n - 1);
    return (x + y)
```

は以下のように変換されます:

```
let rec fibm =
  function
  | 0 -> return 0
  | 1 -> return 1
  | n ->
      (fibm (n - 2)) >>=
        ((fun x  -> (fibm (n - 1)) >>= (fun y  -> return (x + y))))
```

match

```
let rec fibm n = match%monad n with
  | 0 -> return 0
  | 1 -> return 1
  | _ ->
    x <- fibm (n - 2);
    y <- fibm (n - 1);
    return (x + y)
```

は以下のように変換されます:

```
let rec fibm n =
  match n with
  | 0 -> return 0
  | 1 -> return 1
  | _ ->
      (fibm (n - 2)) >>=
        ((fun x  -> (fibm (n - 1)) >>= (fun y  -> return (x + y))))
```

Toplevel let

```
let%monad f xs ys =
  let open List in
  x <- xs;
  y <- ys;
  return (x + y)
```

は以下のように変換されます:

```
let f xs ys =
  let open List in xs >>= (fun x  -> ys >>= (fun y  -> return (x + y)))
```

Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install ppx_monad
uninstall: clean
  opam uninstall ppx_monad

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package ppx_monad ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package ppx_monad ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o ex01 ex01.opt .omakedb* *.omc
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  ppx_monad
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/m2ym/ppx_monad
