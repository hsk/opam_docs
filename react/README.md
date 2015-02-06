# react

リアクティブプログラミングをOCaml上で行います。

## インストール

	$ opam install react

## 使い方

```
let pr_time t =
  let tm = Unix.localtime t in
  Printf.printf "\x1B[8D%02d:%02d:%02d%!"
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

open React;;

let seconds, run =
  let e, send = E.create () in
  let run () =
    while true do
      send (Unix.gettimeofday ());
      Unix.sleep 1
    done
  in
  e, run

let printer = E.map pr_time seconds

let _ = run ()
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install react
uninstall: clean
  opam uninstall react

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package react,unix -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package react,unix -linkpkg ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.react ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  react
  unix
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://github.com/dbuenzli/react

http://erratique.ch/software/react/doc/React.html#basics
