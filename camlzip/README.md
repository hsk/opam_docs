# CamlZip

ZLibのポートです。ZLibを使って圧縮、解凍ができます。

## インストール

	$ opam install camlzip

## 使い方

zlibでの圧縮解凍例です。

```
let compress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.compress (fun buf -> input ic buf 0 (String.length buf))
                (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let uncompress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.uncompress (fun buf -> input ic buf 0 (String.length buf))
                  (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let _ =
  let oc = open_out "test.txt" in
  Printf.fprintf oc "test\n";
  close_out oc;

  compress "test.txt" "test.dat";
  uncompress "test.dat" "test2.txt"
```

さらに詳しく使いたい場合はこちらを参考にしてください。

https://github.com/Leonidas-from-XIV/camlzip/tree/master/test

Makefile

```
default: ex01 ex01.opt minizip minigzip run run.opt

install:
	opam install camlzip
uninstall: clean
	opam uninstall camlzip

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package camlzip zip.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package camlzip zip.cma ex01.ml -o ex01


minizip: minizip.ml
	ocamlfind ocamlc -package camlzip -linkpkg minizip.ml -o minizip
minizip.ml:
	wget https://raw.githubusercontent.com/Leonidas-from-XIV/camlzip/master/test/minizip.ml

minigzip: minigzip.ml
	ocamlfind ocamlc -package camlzip -linkpkg minigzip.ml -o minigzip
minigzip.ml:
	wget https://raw.githubusercontent.com/Leonidas-from-XIV/camlzip/master/test/minigzip.ml

run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.camlzip ex01 ex01.opt .omakedb* *.omc *.run test* minigzip minizip minigzip.ml	minizip.ml
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  camlzip
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

- [gzip.mli](https://github.com/Leonidas-from-XIV/camlzip/blob/master/src/gzip.mli)
- [zip.mli](https://github.com/Leonidas-from-XIV/camlzip/blob/master/src/zip.mli)
- [zlib.mli](https://github.com/Leonidas-from-XIV/camlzip/blob/master/src/zlib.mli)
