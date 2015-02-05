(* parse *)
let _ =

  let json = Yojson.Basic.from_string "{\"aaa\":\"bbb\"}" in
  match json with
  | `Assoc([k,`String v]) ->
    Printf.printf "%s %s\n" k v
  | _ -> assert false

(* to_string *)
let _ =
  let json = `Assoc(["data",`String "v"]) in
  Printf.printf "%s\n" (Yojson.Basic.to_string json)

(* util *)
open Yojson.Basic.Util
let _ =
  let json = Yojson.Basic.from_string "{\"aaa\":\"bbb\",\"bbb\":\"ccc\"}" in
  Printf.printf "%s\n" (json |> member "aaa" |> to_string);
  Printf.printf "%s\n" (json |> member "bbb" |> to_string)

