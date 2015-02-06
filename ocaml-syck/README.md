# OCamlSyck

Yamlのパーサ

## インストール

  $ brew install syck
  $ git clone https://github.com/mk270/ocaml-syck.git
  $ opam install oasis
  $ cd ocaml-syck;oasis setup
  $ cd ocaml-syck;ocaml setup.ml -configure
  $ cd ocaml-syck;ocaml setup.ml -build
  $ cd ocaml-syck;ocaml setup.ml -install


## 使い方

```
let rec spew depth = function
  | YamlNode.SCALAR (uri, value) ->
      Printf.printf "!<%s> %s\n" uri value
  | YamlNode.SEQUENCE (tag, seq) ->
      Printf.printf "spewing SEQUENCE (%s)\n" tag ;
      List.iter
        (fun value ->
           Printf.printf "- " ;
           spew (depth + 1) value)
        seq
  | YamlNode.MAPPING (tag, map) ->
      Printf.printf "spewing MAPPING (%s)\n" tag ;
      List.iter
        (fun (key, value) ->
           Printf.printf "? " ;
           spew (depth + 1) key ;
           Printf.printf ": " ;
           spew (depth + 1) value)
        map

let _ =
  let p = YamlParser.make () in
  let v = YamlParser.parse_string p "---\n- not" in
    spew 0 v
```


Makefile

```
default: ex01 ex01.opt run run.opt

ocaml-syck:
  git clone https://github.com/mk270/ocaml-syck.git

install: ocaml-syck
  brew install syck
  opam install oasis
  cd ocaml-syck;oasis setup
  cd ocaml-syck;ocaml setup.ml -configure
  cd ocaml-syck;ocaml setup.ml -build
  cd ocaml-syck;ocaml setup.ml -install

uninstall:
  ocamlfind remove yaml

ex01.opt: ex01.ml
  ocamlfind ocamlopt -package yaml -linkpkg ex01.ml -o ex01.opt
ex01: ex01.ml
  ocamlfind ocamlc -package yaml -linkpkg ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -rf ocaml-syck *.cm* *.o *.csv ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  yaml
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```
