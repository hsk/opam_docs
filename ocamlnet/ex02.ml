let get url =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let connection = new Curl.handle in
  let result = Buffer.create 16384 in
  connection#set_writefunction (fun data ->
    Buffer.add_string result data;
    String.length data
  );
  connection#set_url url;
  connection#perform;
  Buffer.contents result

let parse_html html = 
    let ch = new Netchannels.input_string html in
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
  let html = get "http://www.google.com/" in
  let docs = parse_html html in
  walk docs
