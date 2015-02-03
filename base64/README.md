# base64

## インストール

	$ opam install base64

## 使い方

```
let _ =
	let base64 = B64.encode "test" in
	Printf.printf "%s\n" base64;
	Printf.printf "%s\n" (B64.decode base64);
```

```
test.opt: test.ml
	ocamlfind ocamlopt -package base64 base64.cmxa test.ml -o test.opt
test: test.ml
	ocamlfind ocamlc -package base64 base64.cma test.ml -o test
oinstall:
	opam install base64
ouninstall:
	opam uninstall base64
	ocamlfind remove base64
clean:
	rm -f *.cm* *.o test test.opt
```

B64.encodeでエンコードします。
B64.decodeでデコードします。

## 参考URL

