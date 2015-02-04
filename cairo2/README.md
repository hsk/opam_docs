# Cairo2

アンチエイリアス付きの画像処理ライブラリ
Cairoより新しいバージョン

## インストール

	$ opam install cairo2

## 使い方

```
let width  = 400
let height = 400

let main = 
  (* Setup Cairo *)
  let surface = Cairo.Image.create Cairo.Image.ARGB32 width height in
  let ctx = Cairo.create surface in

  (* Set thickness of brush *)
  Cairo.set_line_width ctx 15. ;

  (* Draw out the triangle using absolute coordinates *)
  Cairo.move_to     ctx   200.  100. ;
  Cairo.line_to     ctx   300.  300. ;
  Cairo.rel_line_to ctx (-200.)   0. ;
  Cairo.Path.close ctx ;

  (* Apply the ink *)
  Cairo.stroke ctx ;

  (* Output a PNG file *)
  Cairo.PNG.write surface "triangle.png"
```


Makefile

```
default: test test.opt test_font.opt run run.opt

install:
	opam install cairo2
uninstall: clean
	opam uninstall cairo2

test_font.opt: install test_font.ml
	ocamlfind ocamlopt -package cairo2 test_font.ml -o test_font.opt -linkpkg
test.opt: install test.ml
	ocamlfind ocamlopt -package cairo2 cairo2.cmxa test.ml -o test.opt -linkpkg
test: install test.ml
	ocamlfind ocamlc -package cairo2 cairo2.cma test.ml -o test -linkpkg
run: test
	./test
run.opt: test.opt
	./test.opt
	./test_font.opt
clean:
	rm -f *.cm* *.o *.cairo2 test *.opt .omakedb* *.omc *.run *.png
```
or

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= test
FILES2[]= test_font
OCAMLPACKS[]=
  cairo2
.DEFAULT: $(OCamlProgram test, $(FILES)) $(OCamlProgram test_font, $(FILES2))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```
