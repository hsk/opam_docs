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
  write_image img "ex01.jpg"
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install imagemagick
uninstall: clean
  opam uninstall imagemagick

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package magick ex01.ml -o ex01.opt -linkpkg
ex01: install ex01.ml
  ocamlfind ocamlc -package magick ex01.ml -o ex01 -linkpkg
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.imagemagick ex01 ex01.opt .omakedb* *.omc *.run
```
or

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  magick
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/besport/ocaml-imagemagick

- [magick.mli](https://github.com/besport/ocaml-imagemagick/blob/master/magick.mli)
