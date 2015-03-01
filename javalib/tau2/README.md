# tau2 language

BNFCとOCaml,JavaLibを使ったコンパイラ作成の簡単な例です。

```
make
```

で、Tau.classを生成し実行します。

JavaLibの使い辛い点は、配列を用意する所と、ラベルが使えないので元の記事のデータ型から文字列出力の箇所をリストに溜め込みつつ位置情報とラベルの位置情報を表に保存して、JavaLibのバイトコード配列生成時に呼び出す時はその情報を元に組み立てています。


## 参考URL

OCamlで128行で作るJVMバイトコードコンパイラ

http://rainyday.blog.so-net.ne.jp/2014-02-15
