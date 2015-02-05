# OCamlGraph

グラフの処理ライブラリ

## インストール

	$ opam install ocamlgraph

## 使い方

SCC(強連結成分分解)をしてみます。

ex01.ml

```
open Graph

module G = Imperative.Digraph.Abstract(struct type t = string end)
module SCC = Components.Make(G)

let _ =
  let g = G.create() in
  let a = G.V.create "a" in
  let b = G.V.create "b" in
  let c = G.V.create "c" in
  G.add_edge g a b;
  G.add_edge g b a;
  G.add_edge g c a;
  let vss = SCC.scc_list g in
  List.iter begin fun vs ->
    Printf.printf "[%s]\n" (String.concat "," (List.map G.V.label vs))
  end vss
```

インターフェイスはFunctorを使っていて、破壊的です。
内部実装を見るとArrayを使って破壊的なアルゴリズムになっているのが分かります。
使い方は、まず、文字列を使うなら、Imperative.Digraph.Abstractで、stringグラフのモジュールGを作り、Components.MakeでSCC用のモジュールを作ります。
あとは、G.createでグラフを作り、G.V.createでグラフの頂点を作って、G.add_edgeでグラフに頂点を追加します。
SCCを計算して、リストを取得するには、SCC.scc_listを使います。scc_listは２次元のリストを返します。
中味はG.V.tであり、文字列ではないので、文字列に変換するには、G.V.label関数を使うと良いです。

結果が２次元のリストになっているのは、結果が、強連結成分のリストだからです。
この例では、List.iterで外側のリストをループしていて、強連結成分の頂点データから文字列を取り出してString.concatで繋げて表示しています。

Makefile

```
default: ex01 ex01.opt run run.opt

install:
	opam install ocamlgraph
uninstall: clean
	opam uninstall ocamlgraph

ex01.opt: install ex01.ml
	ocamlfind ocamlopt -package ocamlgraph graph.cmxa ex01.ml -o ex01.opt
ex01: install ex01.ml
	ocamlfind ocamlc -package ocamlgraph graph.cma ex01.ml -o ex01
run: ex01
	./ex01
run.opt: ex01.opt
	./ex01.opt
clean:
	rm -f *.cm* *.o *.ocamlgraph ex01 ex01.opt .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  ocamlgraph
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```
