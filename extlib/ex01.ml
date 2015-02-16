let _ =
  Printf.printf "%s\n" (Std.dump 42);
  Printf.printf "%s\n" (String.concat "," 
    (List.map string_of_int (ExtList.List.unique [1;1;2])))

