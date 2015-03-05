open Javalib_pack

let _ =
  let aname = JBasics.make_cn (Sys.argv.(1)) in
  let class_path = Javalib.class_path "./" in

  let a = Javalib.get_class class_path aname in

  Javalib.unparse_class a (open_out "aa.class");
  Javalib.JPrint.print_jasmin a stdout;
