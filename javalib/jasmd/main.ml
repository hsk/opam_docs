let _ =
  let mode = ref `Jasmin in
  let files = ref [] in
  let usage = "Jasmd version 0.0.1 - jvm bytecode decompiler\nUsage: jasmd [options] classfilename" in
  let params = [
      "-jasmin", Arg.Unit(fun () -> mode := `Jasmin), " output jasmin";
      "-high", Arg.Unit(fun () -> mode := `High), " output ocaml high format";
      "-low", Arg.Unit(fun () -> mode := `Low),   " output ocaml low  format";
      "-dumplow", Arg.Unit(fun () -> mode := `DumpLow), " output dumplow";
    ] in
  Arg.parse
    params
    (fun s -> files := !files @ [s])
    usage;
  if !files = [] then (
    print_endline usage;
    List.iter(function (a,_,b) -> print_endline ("  " ^ a ^ " " ^ b)) (params @ [
      "-help", Arg.Unit(fun()->()), " Display this list of options";
      "--help", Arg.Unit(fun()->()),"  Display this list of options";
    ])
  ) else
  List.iter begin fun name ->
    let aname = JBasics.make_cn name in
    let class_path = JFile.class_path "./" in
    let a = JFile.get_class class_path aname in
    begin match !mode with
    | `Jasmin ->
      JPrint.print_jasmin a stdout
    | `High ->
      Format.printf "open Javalib_pack\n";
      Format.printf "open Javalib\n";
      Format.printf "let _ =\nlet k =@.";
      Format.printf "%a@." (PJClass.pp PJCode.pp_jcode) a;
      Format.printf "  in Javalib.unparse_class k (open_out %S);\n" (name ^ ".class");
      Format.printf "  JPrint.print_jasmin k stdout;\n\n"
    | `DumpLow ->
      let l = JHigh2Low.high2low a in
      let out = IO.output_channel stdout in
      JDumpLow.dump out l
    | `Low ->
      let l = JHigh2Low.high2low a in
      Format.printf "%a\n" PJClassLow.pp l
    end
  end !files
