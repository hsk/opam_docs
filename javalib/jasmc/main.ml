open Javalib_pack
open Javalib

let lexbuf l = Parser.jas_file Lexer.token l

let file f =
  let inchan = open_in f in
  begin try
    Parser.sourcefile := Some f;
    let k = lexbuf (Lexing.from_channel inchan) in
    Javalib.unparse_class k (open_out (f ^ ".class"));
    JPrint.print_jasmin k stdout;
    close_in inchan
  with e ->
    close_in inchan;
    raise e
  end

let () =
  let usage = "jasmc version 0.0.1\n  usage: jasmc files" in
  let files = ref [] in
  Arg.parse [] (fun s -> files := !files @ [s]) usage;
  if !files = [] then print_endline usage else
  List.iter (fun f ->
    print_endline @@ "compile " ^ f;
    ignore (file f)
  ) !files
