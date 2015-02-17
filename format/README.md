# Format

文字列をフォーマットして出力する強力なライブラリです。

## インストール

標準ライブラリ

## 使い方

分かり辛い改行とネストは以下のように書くとうまく行きます。

```
let () =
  Format.printf "@[<2>tes(";
    Format.printf "@\n@[<2>tes(";
      Format.printf "@\n@[<2>tes(";
        Format.printf "@\naa";
        Format.printf "@\naa";
        Format.printf "@\naa";
      Format.printf "@]@\n)";
    Format.printf "@]@\n)";
  Format.printf "@]@\n)";
  Format.printf "@."
```

出力結果

```
tes(
  tes(
    tes(
      aa
      aa
      aa
    )
  )
)
```

ポイントは、改行は`@\n`で改行する事と、`@[<2>`と `@]` で括る事です。<2>はいくつネストするかを表します。

※ `\n` `@.` `@?`を使うとうまく動作しなくなるので注意が必要です。

Makefile

```
default: run

install:

uninstall:

run: ex01 ex02 ex03
	./ex01
	./ex02
	./ex03

run.opt: ex01.opt
	./ex01.opt

ex01: ex01.ml
	ocamlc ex01.ml -o ex01
ex02: ex02.ml
	ocamlc ex02.ml -o ex02
ex03: ex03.ml
	ocamlc ex03.ml -o ex03
ex01.opt: ex01.ml
	ocamlopt ex01.ml -o ex01.opt

clean:
	rm -f *.cm* *.o ex01 ex01.opt ex02 ex03
```

## 参考URL

http://ocaml.jp/Format

http://stackoverflow.com/questions/12549411/ocaml-format-and-structural-boxes-why-does-my-output-not-match-the-example

