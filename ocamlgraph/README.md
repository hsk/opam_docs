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

インターフェイスはFunctorを使っています。
使い方は、まず、文字列を使うなら、Imperative.Digraph.Abstractで、stringグラフのモジュールGを作り、Components.MakeでSCC用のモジュールを作ります。

```
open Graph

module G = Imperative.Digraph.Abstract(struct type t = string end)
module SCC = Components.Make(G)

```

G.createでグラフを作り、G.V.createでグラフの頂点を作って、G.add_edgeでグラフに頂点を追加します。

```
let _ =
  let g = G.create() in
  let a = G.V.create "a" in
  let b = G.V.create "b" in
  let c = G.V.create "c" in
  G.add_edge g a b;
  G.add_edge g b a;
  G.add_edge g c a;
```

ここで作成したグラフは、aとbが相互に参照していて、cがaを参照しているグラフです。

SCCを計算して、リストを取得するには、SCC.scc_listを使います。
scc_listは結果として強連結成分のリストを返します。イメージとしては[["a";"b"];["c"]]なリストを返すわけです。

```
  let vss = SCC.scc_list g in
```

しかし、SCC.scc_listの型はG.t -> G.V.t list list です。中味がG.V.tで文字列ではないので、中味の文字列を取り出すのに、G.V.label関数を使います。

```
  List.iter begin fun vs ->
    Printf.printf "[%s]\n" (String.concat "," (List.map G.V.label vs))
  end vss
```

この例では、List.iterで外側のリストをループして、強連結成分の頂点データから文字列を取り出してString.concatで繋げて表示しています。

結果として以下のように表示されます。

```
[a,b]
[c]
```

demoを見ると、dotファイルを生成する等、かなり高機能なようなのですが、とりあえず触りだけ触ってみました。

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

## 参考URL

ocamlgraphについての大体の情報について

http://no-maddojp.hatenablog.com/entry/2013/11/29/163610

Functorの使い方の入り口を把握

http://d.hatena.ne.jp/soutaro/20070103/1167827921

ocamlgraphの github

https://github.com/backtracking/ocamlgraph

Graphの使い方の例をテストから見る

https://github.com/backtracking/ocamlgraph/blob/master/tests/check.ml

componentsの仕様

https://github.com/backtracking/ocamlgraph/blob/master/src/components.mli

vetex labelの仕様

https://github.com/backtracking/ocamlgraph/blob/master/src/sig.mli
