type v =
  | A
  | B of int
  | C of int * string
[@@deriving yojson]

type vs = v list
[@@deriving yojson]

let _ =
  let json = [A;B 1; C(2,"aaa")] in
  Printf.printf "%s\n" (Yojson.Safe.to_string (vs_to_yojson json))


type geo = {
  lat [@key "Latitude"]  : float;
  lon [@key "Longitude"] : float;
}
[@@deriving yojson]

let _ =
  let geo = {lat=1.2; lon=5.55} in
  Printf.printf "%s\n" (Yojson.Safe.to_string (geo_to_yojson geo))
