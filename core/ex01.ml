
open Core.Std

let _ =
  let list = [1;2;3] in
  let rc = List.fold_left list ~init:0 ~f:(fun acc x ->
    acc + x
  ) in
  Printf.printf "%d\n" rc

