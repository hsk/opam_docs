let parse_html_string uri = 
    let s = Http_client.Convenience.http_get uri in
    let ch = new Netchannels.input_string s in
    let docs = Nethtml.parse ?return_pis:(Some false) ch in
    ch # close_in ();
    docs

let rec walk docs =
  List.iter (function
    | Nethtml.Element(tag, attrs, docs) ->

      List.iter (function
        | ("href",v) | ("link",v)->
          Printf.printf "%s\n" v
        | _ -> ()
      ) attrs;
      
  		walk docs
  	| Nethtml.Data a -> ()
  ) docs

let _ =
  let docs = parse_html_string "http://www.caml.org/" in
  walk docs
