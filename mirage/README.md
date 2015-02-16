# MIRAGE OS

OCamlでOSを作るライブラリ

## インストール

	$ opam install mirage

## 使い方

Makefile

```
all: mirage-skeleton/console/mir-console run

install: mirage-skeleton
	opam install mirage
mirage-skeleton/README.md:
	git clone git://github.com/mirage/mirage-skeleton.git
mirage-skeleton/console/mir-console: mirage-skeleton/README.md
	cd mirage-skeleton; cat console/unikernel.ml
	cd mirage-skeleton; cat console/config.ml
	cd mirage-skeleton/console; mirage configure --unix
	cd mirage-skeleton/console; make depend
	cd mirage-skeleton/console; make
	cd mirage-skeleton/console; mirage run
run:
	cd mirage-skeleton/console; ./mir-console
uninstall:
	opam uninstall mirage
clean:
	rm -rf mirage-skeleton
```

## 参考URL

http://openmirage.org/wiki/hello-world

http://qiita.com/mzp/items/3af553c3e81929e04766

