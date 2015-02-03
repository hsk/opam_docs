# xml-light

xmlファイルを読み込み、データを操作し、文字列化します。
dtdによる検証も行う事が出来ます。

## インストール

	$ opam install xml-light

## 使い方

```
let x = Xml.parse_string "<a href='url'>TEXT<begin/><end/></a>" in
    Printf.printf "XML formated = \n%s" (Xml.to_string_fmt x);

    Printf.printf "tag=%s\n" (Xml.tag x);
    Printf.printf "href=%s\n" (Xml.attrib x "href");
    let childs = Xml.children x in
    List.iter (function
      | (Xml.Element _) as x -> Printf.printf "  tag=%s\n" (Xml.tag x)
      | Xml.PCData(x)        -> Printf.printf " text=%s\n" x
    ) childs;


    Printf.printf "tag=%s\n" (Xml.tag x);
    Printf.printf "href=%s\n" (Xml.attrib x "href");
    Xml.iter (fun x ->
      Printf.printf "  %s\n" (Xml.to_string x)
    ) x;

    let x = Xml.parse_file "data.xml" in
    Printf.printf("read dtd\n");

    (try
      let dtd = Dtd.parse_file "data.dtd" in
      Printf.printf("ok\n");
      let x = Dtd.prove (Dtd.check dtd) "start" x in
      print_endline (Xml.to_string x)
    with
    | Xml.Error msg as e ->
      Printf.printf "Xml error : %s\n" (Xml.error msg)
    | Dtd.Parse_error msg as e ->
      Printf.printf "Dtd parse error : %s\n" (Dtd.parse_error msg)
    | Dtd.Check_error msg as e ->
      Printf.printf "Dtd check error : %s\n" (Dtd.check_error msg)
    | Dtd.Prove_error msg as e ->
      Printf.printf "Dtd prove error : %s\n" (Dtd.prove_error msg));
  
```

data.dtd
```
<!ELEMENT start (user)>
<!ELEMENT user (name, sex, body)>
<!ELEMENT name (#PCDATA)>
<!ELEMENT sex (#PCDATA)>
<!ELEMENT body (height, weight)>
<!ELEMENT height (#PCDATA)>
<!ELEMENT weight (#PCDATA)>
```

data.xml
```
<start>
    <user>
        <name>Bob</name>
        <sex>male</sex>
        <body>
            <height>150</height>
            <weight>48</weight>
        </body>
    </user>
</start>
```


Makefile

```
default: test test.opt run run.opt

install:
  opam install xml-light
uninstall: clean
  opam uninstall xml-light

test.opt: install test.ml
  ocamlfind ocamlopt -package xml-light xml-light.cmxa test.ml -o test.opt
test: install test.ml
  ocamlfind ocamlc -package xml-light xml-light.cma test.ml -o test
run: test
  ./test
run.opt: test.opt
  ./test.opt
clean:
  rm -f *.cm* *.o *.csv test test.opt
```
