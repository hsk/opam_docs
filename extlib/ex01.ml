(* base64 *)
let _ =
  Printf.printf "Base64\n";
  let base64 = Base64.str_encode "test" in
  Printf.printf "  %s\n" base64;
  Printf.printf "  %s\n" (Base64.str_decode base64)

(* std *)
let _ =
  Printf.printf "Std\n";
  Printf.printf "  %s\n" (Std.dump 42)

(* list *)
let _ =
  Printf.printf "List\n";
  Printf.printf "  %s\n" (String.concat "," 
    (List.map string_of_int (ExtList.List.unique [1;1;2])))

(* bitset *)
let _ =
  Printf.printf "BitSet\n";
  let s = BitSet.empty () in
  BitSet.set s 10;
  Printf.printf "  %b\n" (BitSet.is_set s 10);
  Printf.printf "  %b\n" (BitSet.is_set s 50);
  BitSet.unset s 10;
  Printf.printf "  %b\n" (BitSet.is_set s 10);
  Printf.printf "  %b\n" (BitSet.is_set s 50)

(* dllist *)
let _ =
  Printf.printf "Dllist\n";
  let dllist = Dllist.of_list[1;2;3;10] in
  Printf.printf "%d\n" (Dllist.get dllist);
  let dllist = Dllist.next dllist in
  Printf.printf "%d\n" (Dllist.get dllist);
  let dllist = Dllist.next dllist in
  Printf.printf "%d\n" (Dllist.get dllist);
  let dllist = Dllist.next dllist in
  Printf.printf "%d\n" (Dllist.get dllist);
  let dllist = Dllist.next dllist in
  Printf.printf "%d\n" (Dllist.get dllist)
