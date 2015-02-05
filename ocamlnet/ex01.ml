let _ =
  let s = Http_client.Convenience.http_get "http://www.caml.org/" in
  Printf.printf "%s\n" s
