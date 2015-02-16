
let parse input =
  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Ollvm_lexer.parse lexbuf in
  close_in inp;
  ast

let _ =
  let ast = parse "test.ll" in
  Ollvm_printer.toplevelentries (Ollvm_printer.empty_env ()) Format.std_formatter ast



