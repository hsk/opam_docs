# camlimages

画像の読み込み、書き込み、文字列描画等を行います。

## インストール

	$ opam install camlimages

## 使い方

```
let _ =
  let img = new OImages.rgb24 20 20 in
  img#save "aa.png" None [];
  img#destroy

let _ =
  let img = new OImages.rgb24_filled 20 20 {Color.r=255;Color.g=0;Color.b=128} in
  img#save "bb.png" None [];
  img#destroy

let _ =
  let rgb24 = Rgb24.create 20 20 in
  let img = OImages.make (Images.Rgb24 rgb24) in
  img#save "cc.png" None [];
  img#destroy

let _ =
  let rgb24 = Rgb24.make 20 20 {Color.r=255;Color.g=0;Color.b=0} in
  let img = OImages.make (Images.Rgb24 rgb24) in
  img#save "dd.png" None [];
  img#destroy

module DrawString = struct
  module FtDraw = Fttext.Make(Rgb24);;

  let draw_string bitmap str x y =
    let library = Freetype.init () in
    let face, face_info = Freetype.new_face library "micap.ttf" 0 in
    Freetype.set_char_size face 18.0 18.0 72 72;
    let str = Fttext.unicode_of_latin str in
    let x1,y1,x2,y2 = Fttext.size face str in
    FtDraw.draw_text face Fttext.func_darken_only bitmap
        (- (truncate x1)) (truncate y2) str

  let show_image img x y =
    Graphics.open_graph "";
    let gr_img = Graphics.make_image (Graphic_image.array_of_image img) in
    Graphics.draw_image gr_img x y;
    let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
    ()

  let _ =
    let rgb24 = Rgb24.make 200 200 {Color.r=255;Color.g=0;Color.b=0} in
    draw_string rgb24 "test" 0 0;
    show_image (Images.Rgb24 rgb24) 200 200;
    let img = OImages.make (Images.Rgb24 rgb24) in
    img#save "dd.png" None [];
    img#destroy
end
```


Makefile

```
default: test test.opt run run.opt

install:
  opam install camlimages
uninstall: clean
  opam uninstall camlimages

test.opt: install test.ml
  ocamlfind ocamlopt -package camlimages.all_formats,camlimages.freetype,camlimages.graphics -linkpkg test.ml -o test.opt
test: install test.ml
  ocamlfind ocamlc -package camlimages.all_formats,camlimages.freetype,camlimages.graphics -linkpkg test.ml -o test

run: test
  ./test
run.opt: test.opt
  ./test.opt
clean:
  rm -f *.cm* *.o *.camlimages test test.opt *.omc *.run .omakedb* *.png
```


# 参考

http://ocaml-checkers.googlecode.com/svn/trunk/camlimages-2.2.0/test/test.ml
