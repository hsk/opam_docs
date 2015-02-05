type e =
  | Int of int
  | Add of e * e
[@@deriving show]

let _ =
  let e = Add(Int 1, Int 2) in
  Printf.printf "e=%s\n" (show_e e);
  Format.fprintf Format.std_formatter "e=%a\n" pp_e e;;
