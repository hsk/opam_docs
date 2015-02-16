let _ =
  let s = Pcre.exec ~rex:(Pcre.regexp "aaa(.*)bbb") "aaaaaabbbccc" in
  let ss = Pcre.get_substrings s in
  let ss = Array.to_list ss in
  List.iter(fun(s) ->
    Printf.printf "%s\n" s
  ) ss
