open Javalib_pack

let _ =
  let aname = JBasics.make_cn "A" in
  let java_lang_string = JBasics.make_cn "java.lang.String" in
  let ms =
    JBasics.make_ms "m" [JBasics.TObject (JBasics.TClass java_lang_string)] None in
  let fs = JBasics.make_fs "f" (JBasics.TBasic `Int) in
  let class_path = Javalib.class_path "./" in

  Printf.printf "get_class\n";

  let a = Javalib.get_class class_path aname in
  Printf.printf "get_method\n";

  let m = Javalib.get_method a ms in
  Printf.printf "get_field\n";
  let f = Javalib.get_field a fs in

  Javalib.unparse_class a (open_out "aa.class");
  Javalib.JPrint.print_jasmin a stdout;
  Javalib.JPrint.print_class a (fun a->Javalib.JPrint.jcode a)stdout;
  Javalib.JPrint.print_method m (fun a->Javalib.JPrint.jcode a)stdout
