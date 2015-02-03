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
default: test test.opt run run.opt

install:
	opam install csv
uninstall: clean
	opam uninstall csv

test.opt: install test.ml
	ocamlfind ocamlopt -package csv csv.cmxa test.ml -o test.opt
test: install test.ml
	ocamlfind ocamlc -package csv csv.cma test.ml -o test
run: test
	./test
run.opt: test.opt
	./test.opt
clean:
	rm -f *.cm* *.o *.csv test test.opt
```
