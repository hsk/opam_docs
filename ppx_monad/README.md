# PPX_deriving

型に自動的に文字列化等の機能を追加します。

## インストール

	$ opam install ppx_deriving

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
default: test test.opt run run.opt

install:
  opam install ppx_monad
uninstall: clean
  opam uninstall ppx_monad

test.opt: install test.ml
  ocamlfind ocamlopt -package ppx_monad test.ml -o test.opt
test: install test.ml
  ocamlfind ocamlc -package ppx_monad test.ml -o test
run: test
  ./test
run.opt: test.opt
  ./test.opt
clean:
  rm -f *.cm* *.o test test.opt
```

## 参考URL

https://github.com/m2ym/ppx_monad
