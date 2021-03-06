let _ =
  let ml = "data.ml" in
  let js = Str.global_replace (Str.regexp "\\.ml$") ".js" ml in
  assert (js = "data.js");
  Format.printf "%s %s@." ml js

let _ =
  let src = "da.ta.ml" in
  let dst = Str.global_replace (Str.regexp "\\.") "_" src in
  assert (dst = "da_ta_ml");
  Format.printf "%s %s@." src dst

let _ =
  let is_c name =
    Str.string_match (Str.regexp ".*\\.c$") name 0
  in
  let name = "data.c" in
  assert (is_c name);
  Format.printf "is c %s ? %b@." name (is_c name);
  let name = "data.cpp" in
  assert (not(is_c name));
  Format.printf "is c %s ? %b@." name (is_c name)

let _ =
  let lex src =
    if Str.string_match (Str.regexp "[1-9][0-9]*") src 0 then
      ((Str.matched_string src),(Str.string_after src (Str.match_end())))
    else
      ("",src)
  in
  assert((lex "123 456") = ("123", " 456") );
  assert((lex " 456") = (""," 456") )

let _ =
  let lex src =
    if Str.string_match (Str.regexp "[ \r\n\t]*[1-9][0-9]*") src 0 then
      ((Str.matched_string src),(Str.string_after src (Str.match_end())))
    else
      ("",src)
  in
  assert((lex "123  456") = ("123", "  456") );
  assert((lex "  \n\t456") = ("  \n\t456","") )

let _ =
  let lex src =
    let src = Str.replace_first (Str.regexp "[ \r\n\t]*") "" src in
    if Str.string_match (Str.regexp "[1-9][0-9]*") src 0 then
      ((Str.matched_string src),(Str.string_after src (Str.match_end())))
    else
      ("",src)
  in
  assert((lex "123  456") = ("123", "  456") );
  assert((lex "  \n\t456") = ("456","") );
