# cow

HTMLをOCamlのDSLとして表現する

## インストール

	$ opam install cow

## 使い方

Makefile

```
all: run

install:
	opam install cow
uninstall:
	opam uninstall cow

clean:
	rm -rf ocaml-cow

run: ocaml-cow/tests/Makefile
	cd ocaml-cow && make tests
ocaml-cow/tests/Makefile:
	git clone https://github.com/mirage/ocaml-cow.git
```

## 参考URL

[csv.mli](https://github.com/Chris00/ocaml-csv/blob/master/src/csv.mli)

