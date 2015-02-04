# Cairo

アンチエイリアス付きの画像処理ライブラリ

Cairo2が最新なので、[Cairo2](../cairo)を使うと良いでしょう。
Cairo2とCairoはインターフェイスが若干異なります。

## インストール

libgtk2が必要なので、osxの場合は、homebrewだと
```
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/X11/lib/pkgconfig:$PKG_CONFIG_PATH
```

といった設定が必要です。また、以下のインストールが必要です。

```
brew install gtk+ cairo
```

	$ opam install cairo

## 使い方

```
let width  = 400
let height = 400

let main = 
  (* Setup Cairo *)
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
  let ctx = Cairo.create surface in

  (* Set thickness of brush *)
  Cairo.set_line_width ctx 15. ;

  (* Draw out the triangle using absolute coordinates *)
  Cairo.move_to     ctx   200.  100. ;
  Cairo.line_to     ctx   300.  300. ;
  Cairo.rel_line_to ctx (-200.)   0. ;
  Cairo.close_path  ctx ;

  (* Apply the ink *)
  Cairo.stroke ctx ;

  (* Output a PNG file *)
  Cairo_png.surface_write_to_file surface "triangle.png"
```


Makefile

```
default: test test.opt test_font.opt run run.opt

install:
  opam install cairo
uninstall: clean
  opam uninstall cairo

test_font.opt: install test_font.ml
  ocamlfind ocamlopt -package cairo test_font.ml -o test_font.opt -linkpkg
test.opt: install test.ml
  ocamlfind ocamlopt -package cairo cairo.cmxa test.ml -o test.opt -linkpkg
test: install test.ml
  ocamlfind ocamlc -package cairo cairo.cma test.ml -o test -linkpkg
run: test
  ./test
run.opt: test.opt
  ./test.opt
  ./test_font.opt
clean:
  rm -f *.cm* *.o *.cairo test *.opt .omakedb* *.omc *.run *.png
```
or

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= test
FILES2[]= test_font
OCAMLPACKS[]=
  cairo
.DEFAULT: $(OCamlProgram test, $(FILES)) $(OCamlProgram test_font, $(FILES2))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```
