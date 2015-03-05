open Javalib_pack

let _ =
  let mode = ref `Jasmin in
  let endflg = ref false in
  let files = ref [] in
  let usage = "Jasmd version 0.0.1 - jvm bytecode decompiler\nUsage: jasmd [options] classfilename" in
  let params = [
      "-jasmin", Arg.Unit(fun () -> mode := `Jasmin), " output jasmin";
      "-high", Arg.Unit(fun () -> mode := `High), " output ocaml high format";
      "-low", Arg.Unit(fun () -> mode := `Low),   " output ocaml low  format";
      "-dumplow", Arg.Unit(fun () -> mode := `DumpLow), " output dumplow";
      "-sig", Arg.String(fun s ->

        let f name =
          Format.printf "------ test JParseSignature.parse_%s@." name;
          name
        in

        let name = f "objectType" in
        begin try
          let object_type = JParseSignature.parse_objectType s in
          Format.printf "%a\n" PJBasics.pp_object_type object_type;
        with
          | _ -> Format.printf "error\n"
        end;
        
        let name = f "method_descriptor" in
        begin try
          let method_descriptor = JParseSignature.parse_method_descriptor s in
          Format.printf "%a\n" PJBasics.pp_method_descriptor method_descriptor
        with
          | _ -> Format.printf "error\n"
        end;

        let name = f "field_descriptor" in
        begin try
          let value_type = JParseSignature.parse_field_descriptor s in
          Format.printf "%a\n" PJBasics.pp_value_type value_type;
        with
          | _ -> Format.printf "error\n"
        end;

        let name = f "descriptor" in
        begin try
          let descriptor = JParseSignature.parse_descriptor s in
          Format.printf "%a\n" PJBasics.pp_descriptor descriptor;
        with
          | _ -> Format.printf "error\n"
        end;

        let name = f "ClassSignature" in
        begin try
          let classSignature = JParseSignature.parse_ClassSignature s in
          Format.printf "%a\n" PJSignature.pp_classSignature classSignature;
        with
          | _ -> Format.printf "error\n"
        end;

        let name = f "MethodTypeSignature" in
        begin try
          let methodTypeSignature = JParseSignature.parse_MethodTypeSignature s in
          Format.printf "%a\n" PJSignature.pp_methodTypeSignature methodTypeSignature;
        with
          | _ -> Format.printf "error\n"
        end;

        let name = f "FieldTypeSignature" in
        begin try
          let fieldTypeSignature = JParseSignature.parse_FieldTypeSignature s in
          Format.printf "%a\n" PJSignature.pp_fieldTypeSignature fieldTypeSignature;
        with
          | _ -> Format.printf "error\n"
        end;
        endflg:=true
      ), "parse test any signature\n" ^
         "    etc...  jasmd -sig \"I\"    jasmd -sig \"(IB)V\"";

    ] in
  Arg.parse
    params
    (fun s -> files := !files @ [s])
    usage;
  if !endflg then () else
  if !files = [] then (
    print_endline usage;
    List.iter(function (a,_,b) -> print_endline ("  " ^ a ^ " " ^ b)) (params @ [
      "-help", Arg.Unit(fun()->()), " Display this list of options";
      "--help", Arg.Unit(fun()->())," Display this list of options";
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
