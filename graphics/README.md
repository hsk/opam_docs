# graphics

標準のグラフィックスライブラリです。

バックグラウンド処理や、アンチエイリアシングありの高度な処理を行いたい場合は、[../cairo2](Cairo2)や[../imagemagick](ImageMagick)を使うと良いでしょう。

## インストール

OCamlをインストールすればついてきます。

## 使い方

```
let _ =
  Graphics.open_graph " 640x480";
  Graphics.display_mode false;
  for x = 0 to 50000 do
    let x = Random.int 640 in
    let y = Random.int 480 in
    Graphics.plot x y
  done;
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  ()

let _ =
  Graphics.set_color (Graphics.rgb 128 255 255);
  Graphics.clear_graph ();
  for x = 0 to 50000 do
    let x = Random.int 640 in
    let y = Random.int 480 in
    Graphics.plot x y
  done;
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  Graphics.close_graph()
```


Makefile

```
default: test test.opt run

test.opt: test.ml
  ocamlfind ocamlopt graphics.cmxa test.ml -o test.opt
test: test.ml
  ocamlfind ocamlc graphics.cma test.ml -o test
run: test
  ./test
run.opt: test.opt
  ./test.opt
clean:
  rm -f *.cm* *.o test test.opt .omakedb* *.omc *.run
```
or

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= test
OCAMLPACKS[]=
  csv
.DEFAULT: $(OCamlProgram test, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

http://ocaml.jp/Chapter%2025%20The%20graphics%20library

