let _ =
	let md5 = Digest.string "test" in
	Printf.printf "%s\n" md5;
	Printf.printf "%s\n" (Digest.to_hex md5);

