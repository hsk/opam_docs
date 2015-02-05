type a = [%import: Ex01_data.a] [@@deriving show]
type b = [%import: Ex01_data.b]

let _ =
	let a = C(A, B "test") in
	Printf.printf "%s\n" (show_a a)
