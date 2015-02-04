# PPX_monadic

モナド用の構文を追加します。
[../ppx_monad](ppx_monad)と似てますが、構文が別です。
作者はcamlspotterさんです。

## インストール

	$ opam install ppx_monadic

## 使い方

```
module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

let t1 = 
  let open Option in do_
  ; x <-- return 1
  ; return x
```

Makefile

```
default: test test.opt run run.opt

install:
	opam install ppx_monadic
uninstall: clean
	opam uninstall ppx_monadic

test.opt: install test.ml
	ocamlfind ocamlopt -package ppx_monadic test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package ppx_monadic test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt
```

## 参考URL

https://bitbucket.org/camlspotter/ppx_monadic

