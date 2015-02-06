# OCamlNet

インターネットプロトコル（http、cgi、emailなど）およびヘルパーデータ構造（メールメッセージ、文字セットなど）

## インストール

	$ opam install ocamlnet

## 使い方

htmlデータの取得

```
let _ =
  let s = Http_client.Convenience.http_get "http://www.caml.org/" in
  Printf.printf "%s\n" s
```

htmlのスクレイピング

```
let parse_html_string uri = 
    let s = Http_client.Convenience.http_get uri in
    let ch = new Netchannels.input_string s in
    let docs = Nethtml.parse ?return_pis:(Some false) ch in
    ch # close_in ();
    docs

let rec walk docs =
  List.iter (function
    | Nethtml.Element(tag, attrs, docs) ->

      List.iter (function
        | ("href",v) | ("link",v)->
          Printf.printf "%s\n" v
        | _ -> ()
      ) attrs;
      
      walk docs
    | Nethtml.Data a -> ()
  ) docs

let _ =
  let docs = parse_html_string "http://www.caml.org/" in
  walk docs
```

Makefile

```
default: ex01 ex01.opt ex02 run

install:
  opam install ocamlnet
uninstall: clean
  opam uninstall ocamlnet

ex01.opt: install ex01.ml
  ocamlfind ocamlopt -package netclient -linkpkg ex01.ml -o ex01.opt
ex01: install ex01.ml
  ocamlfind ocamlc -package netclient -linkpkg ex01.ml -o ex01
ex02: install ex02.ml
  ocamlfind ocamlc -package netclient,netstring -linkpkg ex02.ml -o ex02
run: ex01 ex02
  ./ex02
clean:
  rm -f *.cm* *.o *.ocamlnet ex01 ex01.opt ex02 .omakedb* *.omc *.run
```

OMakefile

```
.PHONY: all clean
USE_OCAMLFIND = true
FILES[]= ex01
FILES2[]= ex02
OCAMLPACKS[]=
  netclient
.DEFAULT: $(OCamlProgram ex01, $(FILES))
          $(OCamlProgram ex02, $(FILES2))
clean:
  rm -f $(filter-proper-targets $(ls R, .))
```


## 参考URL

http://ocamlnet.sourceforge.net/manual/refman/

http://ymotongpoo.hatenablog.com/entry/20100225/1267067912
