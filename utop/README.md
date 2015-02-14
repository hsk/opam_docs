# utop

UTOPはカラフルで便利なocamlのreplです。
コマンドラインツールです。

## インストール

	$ opam install utop

## 使い方

```
$ utop
──────────────┬────────────────────────────────────────────────────────────┬───────────────              │ Welcome to utop version 1.17 (using OCaml version 4.01.0)! │               
              └────────────────────────────────────────────────────────────┘               

Type #utop_help for help about using utop.

─( 00:25:37 )─< command 0 >─────────────────────────────────────────────────{ counter: 0 }─
utop # 
┌───┬────────────┬─────┬───────────┬──────────────┬───────┬────────┬──────┬────────┬──────┐
│Arg│Arith_status│Array│ArrayLabels│Assert_failure│Big_int│Bigarray│Buffer│Callback│Camlin│
└───┴────────────┴─────┴───────────┴──────────────┴───────┴────────┴──────┴────────┴──────┘
```

と表示されます。List.foと入力すると

```
utop # List.fo
┌─────────┬──────────┬──────────┬───────────┬───────┬────────┬────────────────────────────┐
│fold_left│fold_left2│fold_right│fold_right2│for_all│for_all2│                            │
└─────────┴──────────┴──────────┴───────────┴───────┴────────┴────────────────────────────┘
```

このように表示されるので便利です。
タブで補完等も出来ます。


Makefile

```
default: install run

install:
        opam install utop
uninstall:
        opam uninstall utop

run:
        utop
```

## 参考URL

https://realworldocaml.org/v1/en/html/a-guided-tour.html

