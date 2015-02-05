type a = [%import: Ex01_data.a]
type b = [%import: Ex01_data.b]

let _ =
	let a = C(A, B "test") in
	match a with
	| C(_,B s) -> Printf.printf "%s\n" s
	| _ -> assert false
