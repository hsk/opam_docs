# Digest 

MD5を扱うライブラリです。

## インストール

標準ライブラリ

## 使い方

```
let _ =
	let md5 = Digest.string "test" in
	Printf.printf "%s\n" md5;
	Printf.printf "%s\n" (Digest.to_hex md5);
```

```
default: test test.opt run run.opt

install:
uninstall: clean

test.opt: install test.ml
	ocamlfind ocamlopt test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o test test.opt
```


## 参考URL

