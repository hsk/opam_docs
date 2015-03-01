# JavaLib

Javaのクラスファイルの読み込み、書き込み、操作、Jasmin形式の書き出しが行えるライブラリです。

JavaLibは読み込みの際にバイトコードまではパースしません。lazyなパースをするため必要があって初めてバイトコードをパースします。

出力するだけなら、baristaライブラリのほうが高速でしょうが、opamでインストール出来ないのでjavalibのほうが便利です。



## インストール

	$ opam install javalib

## 使い方

```java
public class A {

  public int f;

  public void m (String s) {
  }
}
```
<center>A.java</center>

このようなファイルがあった場合に、

```
javac A.java
```

としてコンパイルしておき、


```ocaml
open Javalib_pack

let _ =
  (* クラス名 *)
  let aname = JBasics.make_cn "A" in
  (* Stringのクラス名 *)
  let java_lang_string = JBasics.make_cn "java.lang.String" in

  (* メソッドシグニチャ *)
  let ms =
    JBasics.make_ms "m" [JBasics.TObject (JBasics.TClass java_lang_string)] None in

  (* フィールドシグニチャ *)
  let fs = JBasics.make_fs "f" (JBasics.TBasic `Int) in

  (* Javaのクラスパス *)
  let class_path = Javalib.class_path "./" in

  (* classファイルを読み込む *)
  Printf.printf "get_class\n";

  let a = Javalib.get_class class_path aname in

  (* メソッドを読み込む *)
  Printf.printf "get_method\n";

  let m = Javalib.get_method a ms in

  (* フィールドを読み込む *)
  Printf.printf "get_field\n";
  let f = Javalib.get_field a fs in

  (* クラスファイル出力 *)
  Javalib.unparse_class a (open_out "aa.class");

  (* jasminで出力 *)
  Javalib.JPrint.print_jasmin a stdout;

  (* jasmin形式でclassのみ出力 *)
  Javalib.JPrint.print_class a (fun a->Javalib.JPrint.jcode a)stdout;

  (* jasmin形式でメソッドのみ出力 *)
  Javalib.JPrint.print_method m (fun a->Javalib.JPrint.jcode a)stdout
```
<center>ex01.ml</center>

をコンパイルして実行すると、

```
.bytecode 52.0
.source A.java

.class  public A
.super java.lang.Object

.field public f I 

.method public <init>()V
  .limit stack 1
  .limit locals 1
  .line 1
    0:  aload_0
    1:  invokenonvirtual java/lang/Object/<init>()V
    4:  return
.end method

.method public m(Ljava/lang/String;)V
  .limit stack 0
  .limit locals 2
  .line 4
    0:  return
.end method
```

のような出力が得られます。

クラスファイルを一から作るには以下のような形で作成します。

```
open Javalib_pack

let make_fields class_name =
  let fields = JBasics.FieldMap.empty in
  let field_signature = JBasics.make_fs "f" (JBasics.TBasic `Int) in
  let class_field_signature = JBasics.make_cfs class_name field_signature in
  let class_field = {
    Javalib.cf_signature= field_signature;
    cf_class_signature = class_field_signature;
    cf_generic_signature = None;
    cf_access = `Default;
    cf_static = false;
    cf_synthetic = false;
    cf_enum = false;
    cf_kind = Javalib.NotFinal;
    cf_value = None;
    cf_transient = false;
    cf_annotations = [];
    cf_other_flags = [];
    cf_attributes = { 
      Javalib.synthetic = false;
      deprecated = false;
      other = [];
    };
  } in
  let fields = JBasics.FieldMap.add field_signature class_field fields in
  fields

let make_methods class_name super_class_name =
  let methods = JBasics.MethodMap.empty in
  let java_lang_string = JBasics.make_cn "java.lang.String" in
  let method_signature = JBasics.make_ms "m" [JBasics.TObject (JBasics.TClass java_lang_string)] None in
  let class_method_signature = JBasics.make_cms class_name method_signature in
  let jmethod = {
    JCode.c_max_stack = 0;
    c_max_locals = 2;
    c_code = [|JCode.OpReturn `Void|];
    c_exc_tbl = [];
    c_line_number_table = None;
    c_local_variable_table = None;
    c_local_variable_type_table = None;
    c_stack_map_midp = None;
    c_stack_map_java6 = None;
    c_attributes = [];
  } in
  let rec concrete_method = Javalib.ConcreteMethod {
    Javalib.cm_signature = method_signature;
    cm_class_method_signature = class_method_signature;
    cm_static = false;
    cm_final = false;
    cm_synchronized = false;
    cm_strict = false; 
    cm_access = `Public;
    cm_generic_signature = None;
    cm_bridge = false; 
    cm_varargs = false;
    cm_synthetic = false;
    cm_other_flags = [];
    cm_exceptions = [];
    cm_attributes = {
      Javalib.synthetic = false;
      deprecated = false;
      other = []
    };
    cm_annotations = {
      Javalib.ma_global = [];
      ma_parameters = []
    }; 
    cm_implementation = Javalib.Java (lazy jmethod)
  } in

  let methods = JBasics.MethodMap.add method_signature concrete_method methods in
  methods

let make_jclass class_name super_class_name fields methods =
  Javalib.JClass {
    Javalib.c_name = class_name;
    c_version = {
      JBasics.major = 52;
      minor = 0
    }; 
    c_access = `Public;
    c_final = false;
    c_abstract = false;
    c_super_class = Some super_class_name; 
    c_generic_signature = None;
    c_fields = fields;
    c_interfaces = [];
    c_consts = [||];
    c_sourcefile = Some "B.java";
    c_deprecated = false;
    c_enclosing_method = None; 
    c_source_debug_extention = None;
    c_inner_classes = [];
    c_synthetic = false; 
    c_enum = false;
    c_annotations = [];
    c_other_flags = [];
    c_other_attributes = []; 
    c_methods = methods
  }

let _ =

  let class_name = JBasics.make_cn "B" in
  let super_class_name = JBasics.make_cn "Object" in
  let fields = make_fields class_name in
  let methods = make_methods class_name super_class_name in

  let jclass = make_jclass class_name super_class_name fields methods in

  Javalib.JPrint.print_jasmin jclass stdout;
  Javalib.unparse_class jclass (open_out "B.class");
```
<center>ex02.ml</center>

ちょっと大変そうなので、ex03.mlのようなプログラムでppx_deriving.showを使って構文木を出力するプログラムを作成してみると
全データタイプが公開されていないのでうまく行きません。ppxが動かすのも大変な環境もありそうなので、構文木を出力するプログラムをex5.mlに作成しました。
このプログラムでcalc.javaを出力した結果がex6.mlでコンパイルしてclassファイルを作成出来ます。

tau2ディレクトリには、javalibを使って、簡単なJVMのバイトコードコンパイラがあります。

Makefile

```
default: A.class ex01 ex01.opt ex5 run run.opt


install:
  opam install javalib
uninstall: clean
  opam uninstall javalib

A.class: A.java
  javac A.java

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package javalib -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package javalib -linkpkg ex01.ml -o ex01
ex5: ex5.ml
  ocamlfind ocamlc -package javalib -linkpkg ex5.ml -o ex5
run: ex01
  ./ex01
run.opt: ex01.opt
  ./ex01.opt
clean:
  rm -f *.cm* *.o *.csv ex01 ex01.opt .omakedb* *.omc *.run ex02 ex03 ex5 ex6
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
OCAMLPACKS[]=
  javalib
.DEFAULT: $(OCamlProgram ex01, $(FILES))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```

## 参考URL


