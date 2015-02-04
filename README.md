# OPAM docs

これは何？

OCamlには沢山のライブラリが存在しています。
しかし、ぱっと使ってみたいと思ってもすぐに使えないと不便です。

そこで、OCamlのOPAMパッケージを使った簡単な例を揃えてみました。
Makefileを用意して、インストール、アンインストールと、ocamlcとocamloptでのリンクをする例を乗せました。
makeをするだけで、テストプログラムが動作します。make installでパッケージのインストールをmake uninstallでパッケージの削除が書いてあります。make cleanをすれば不要なファイルを消す事が出来ます。
OMakefileはomakeでのビルドとcleanのみ対応していますが、全てを消すことはできないのでmake cleanで消すようにしました。

## 標準ライブラリ

1. [digest.md](digest) MD5文字列の生成、変換
1. [graphics](graphics) 標準の画像処理ライブラリ

## ppx

1. [ppx_deriving](ppx_deriving) データの自動文字列化等
1. [ppx_monad](ppx_monad) モナド構文の追加
1. [ppx_monadic](ppx_monadic) モナド構文の追加

1. ppx_blob                      --  Include a file as a string at compile time
1. ppx_deriving                 2.0  Type-driven code generation for OCaml >=4.02
1. ppx_deriving_protobuf         --  A Protocol Buffers codec generator for OCaml >=4.02
1. ppx_deriving_yojson           --  JSON codec generator for OCaml >=4.02
1. ppx_import                    --  A syntax extension for importing declarations from interfaces
1. ppx_include                   --  Include OCaml source files in each other
1. ppx_meta_conv                 --  ppx_meta_conv, ppx based type_conv for various tree data formats.
1. ppx_monad                  0.0.1  A monad syntax extension
1. ppx_monadic                   --  ppx_monadic: ppx extension for do notation, pattern guards, and monad comprehension
1. ppx_overload                  --  ppx_overload: SML style simple but user definable overloading
1. ppx_pattern_guard             --  ppx_pattern_guard: ppx extension for pattern guard
1. ppx_poly_record               --  ppx for polymorphic records
1. ppx_test                      --  A ppx replacement of pa_ounit.
1. ppx_tools                 0.99.2  Tools for authors of ppx rewriters and other syntactic tools


## データ処理

1. [base64](base64) 64文字で表す
1. [csv](csv) カンマ区切りのデータの読み書き
1. [xml-light](xml-light) XMLのパース、文字列化、DTDによる検証

## 画像処理

1. [graphics](graphics) 標準の画像処理ライブラリ
1. [camlimages](camlimages) 画像の読込、保存、文字列描画、表示
1. [cairo](cairo) アンチエイリアス付き画像生成、文字列描画、保存
1. [cairo2](cairo2) Cairo最新版アンチエイリアス付き画像生成、文字列描画、保存
1. [imagemagick](imagemagick) 様々な画像の読み込み、保存、描画
[](1. [lablgl](lablgl) OpenGL)
