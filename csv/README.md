# CSV

CSVファイルを2次元の文字列リストとして読み書きしたり、文字列から読み込んだりします。

## インストール

	$ opam install csv

## 使い方

```
let _ =

  Printf.printf "list to csv\n";
  let csv = [["aaa";"bbb";"ccc"];["kkk"]] in
  Csv.print_readable csv;
  Printf.printf "------\n";

  Printf.printf "save and load\n";
  Csv.save "a.csv" csv;
  let csv = Csv.load "a.csv" in
  Csv.print_readable csv;
  Printf.printf "------\n";

  Printf.printf "read string\n";
  let csv = Csv.input_all(Csv.of_string "csv,data,aaa") in
  Csv.print_readable csv
```


Makefile

```
default: ex01 ex01.opt run run.opt

install:
	opam install csv
uninstall: clean
	opam uninstall csv

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package csv csv.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package csv csv.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.csv ex01 ex01.opt
```
or

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  csv
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL

[csv.mli](https://github.com/Chris00/ocaml-csv/blob/master/src/csv.mli)

