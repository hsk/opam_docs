let _ =
	let base64 = B64.encode "test" in
	Printf.printf "%s\n" base64;
	Printf.printf "%s\n" (B64.decode base64);

