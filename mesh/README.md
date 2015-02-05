# Mesh

Mesh


![](http://www-dinma.univ.trieste.it/nirftc/research/easymesh/example10.gif)

の2Dのポリゴンデータを

![](http://www-dinma.univ.trieste.it/nirftc/research/easymesh/example11.gif)

に変換するような処理を行うメッシュを扱うライブラリです。

詳細は分かりませんが、C言語のソースであったものを、移植あるいは呼び出しているようです。

## インストール

	$ opam install Mesh

## 使い方

```
$ wget http://www-dinma.univ.trieste.it/nirftc/research/easymesh/examples.tar.gz
$ tar xvfz examples.tar.gz
x example1.d
x example2.d
x example3.d
x example4.d
x example5.d
x example6.d
x example7.d
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
  opam install mesh
uninstall: clean
  opam uninstall mesh

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package mesh mesh.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package mesh mesh.cma ex01.ml -o ex01
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.mesh ex01 ex01.opt .omakedb* *.omc *.run

info:
  find `ocamlfind printconf path` | grep mesh | grep ".mli" | xargs cat 
  find `ocamlfind printconf path` | grep mesh | grep ".mli" 
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  mesh
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

https://forge.ocamlcore.org/projects/mesh/
http://math.umons.ac.be/an/en/software/
http://www-dinma.univ.trieste.it/nirftc/research/easymesh/
http://www-dinma.univ.trieste.it/nirftc/research/easymesh/easymesh.html
