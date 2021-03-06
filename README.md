# OPAM docs

これは何？

OCamlには沢山のライブラリが存在しています。
OSX、brewの環境で、ぱっと使ってみたいと思ってもすぐに使えるように、OCamlのOPAMパッケージを使った簡単な例を揃えたものです。

http://ocaml.org/docs/

こちらと名前がかぶってよくない事が分かったので、リポジトリ名を変更しようかと検討中です。

```
$ git clone https://github.com/hsk/opam_docs.git
$ cd digest
$ make
```

等とする事で実行してみる事が出来ます。opamのインストールも自動で行います。make uninstallで削除できます。

## 標準ライブラリ

1. [digest](digest) MD5文字列の生成、変換
1. [graphics](graphics) 標準の画像処理ライブラリ
1. [str](str) 標準の正規表現ライブラリ

## 補強ライブラリ

1. [core](core)
2. [extlib](extlib)

## ppx

1. [ppx_blob](ppx_blob)         Include a file as a string at compile time
1. [ppx_deriving](ppx_deriving) データの自動文字列化等 for OCaml >=4.02
1. [ppx_monad](ppx_monad) モナド構文の追加
1. [ppx_monadic](ppx_monadic) モナド構文の追加
1. [ppx_deriving_yojson](ppx_deriving_yojson) データのJSON化
1. ppx_deriving_protobuf         --  A Protocol Buffers codec generator for OCaml >=4.02
1. [ppx_import](ppx_import)                    --  A syntax extension for importing declarations from interfaces
1. ppx\_include                   --  Include OCaml source files in each other
1. ppx\_meta\_conv                 --  ppx\_meta\_conv, ppx based type\_conv for various tree data formats.
1. ppx\_overload                  --  ppx\_overload: SML style simple but user definable overloading
1. ppx\_pattern\_guard             --  ppx\_pattern\_guard: ppx extension for pattern guard
1. ppx\_poly\_record               --  ppx for polymorphic records
1. ppx\_test                      --  A ppx replacement of pa\_ounit.
1. ppx\_tools                 0.99.2  Tools for authors of ppx rewriters and other syntactic tools

## Comminucate

1. biniou                        --  Binary data format designed for speed, safety, ease of use and backward compatibility as protocols evolve


## データ処理

1. [base64](base64) 64文字で表す
1. [csv](csv) カンマ区切りのデータの読み書き
1. [xml-light](xml-light) XMLのパース、文字列化、DTDによる検証
1. [ocamlgraph](ocamlgraph) グラフの計算、表示等
1. [camlzip](camlzip) zipファイルの圧縮、解凍等
1. [yojson](yojson) jsonの読み書き、ソート、文字列化等
1. [ocaml-syck](ocaml-syck) yamlの読み書き

## テキスト処理

1. [pcre](pcre) Perl互換正規表現ライブラリ

## 画像処理

1. [graphics](graphics) 標準の画像処理ライブラリ
1. [camlimages](camlimages) 画像の読込、保存、文字列描画、表示
1. [cairo](cairo) アンチエイリアス付き画像生成、文字列描画、保存
1. [cairo2](cairo2) Cairo最新版アンチエイリアス付き画像生成、文字列描画、保存
1. [imagemagick](imagemagick) 様々な画像の読み込み、保存、描画
[](1. [lablgl](lablgl) OpenGL)
1. [mparser](mparser) シンプルなモナディックパーサコンビネータライブラリ

## ネットワーク

1. [ocurl](ocurl) libcurlのOCamlバインディング
1. [ocamlnet](ocamlnet) http等
1. [cow](cow) HTMLのDSL

## 数学

1. zarith                        --  Implements arithmetic and logical operations over arbitrary-precision integers
1. [mesh](mesh)                  --  Triangular mesh generation and manipulation.
1. odepack
1. l-bfgs
1. 1d root finding
1. boolean expression simplifier
1. tptp                          --  Library for reading and writing FOF and CNF formulas in TPTP format
1. 1d optimazation
19. pareto                        --  GSL powered OCaml statistics library, which provides


## データベース

1. mysql                         --  Bindings to libmysqlclient for interacting with mysql databases
1. mysql_protocol                --  Implementation of MySQL Protocol with the Bitstring library
1. odbc                          --  Interface to various ODBC drivers
1. pgocaml                       --  Interface to PostgreSQL databases
1. postgresql                    --  postgresql-ocaml - bindings to the PostgreSQL library
1. mongo                         --  OCaml driver for MongoDB
1. obigstore                     --  Client/server + embeddable semi-structured database.
1. ocamldbi                      --  Database independent layer patterned upon Perl DBI

## リアクティブプログラミング

1. [react](react)
1. [rml](rml) ReactiveML Language

## コマンドラインツール

1. ocamlc ocamlopt 標準のコンパイラ
1. ocamllex ocamlyacc 標準のコンパイラコンパイラ サードパーティ製ではmenhir ulexがある。
1. camlp4 プリプロセッサ
1. omake OCaml製で、OCamlやC言語等のmakeに変わる便利なビルドツール
1. ocamlfind パッケージ自動検索
1. ocamlspot ソースコードの情報取得
1. [utop](utop) トップレベルコマンド
1. [depext](depext) opam-depext opamのライブラリが依存する外部ライブラリをインストール

## より詳しい説明

Makefileを用意して、インストール、アンインストールと、ocamlcとocamloptでのリンクをする例を乗せました。
makeをするだけで、テストプログラムが動作します。make installでパッケージのインストールをmake uninstallでパッケージの削除が書いてあります。make cleanをすれば不要なファイルを消す事が出来ます。
OMakefileはomakeでのビルドとcleanのみ対応していますが、全てを消すことはできないのでmake cleanで消すようにしました。
