# ImageMagick

画像処理をする定番のライブラリと言えばImageMagickです。

## インストール

	$ opam install imagemagick

## 使い方

```
open Magick
open Imper

let () =
  let img = get_canvas ~width:400 ~height:300 ~color:"#C8E0FF" in

  (* The clouds *)
  List.iter (fun (cx, cy) ->
    draw_ellipse img ~cx ~cy
        ~rx:50 ~ry:20 ~fill_color:(color_of_string "#FFFB") ()
  ) [ (20, 40); (60, 55); (90, 30); (120, 65); (175, 35); ];
  write_image img "test.jpg"
```


Makefile

```
default: test test.opt run run.opt

install:
  opam install imagemagick
uninstall: clean
  opam uninstall imagemagick

test.opt: install test.ml
  ocamlfind ocamlopt -package magick test.ml -o test.opt -linkpkg
test: install test.ml
  ocamlfind ocamlc -package magick test.ml -o test -linkpkg
run: test
  ./test
run.opt: test.opt
  ./test.opt
clean:
  rm -f *.cm* *.o *.imagemagick test test.opt .omakedb* *.omc *.run
```
or

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= test
OCAMLPACKS[]=
  magick
.DEFAULT: $(OCamlProgram test, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/besport/ocaml-imagemagick
