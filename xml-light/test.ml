let x = Xml.parse_string "<a href='url'>TEXT<begin/><end/></a>" in
    Printf.printf "XML formated = \n%s" (Xml.to_string_fmt x);

    Printf.printf "tag=%s\n" (Xml.tag x);
    Printf.printf "href=%s\n" (Xml.attrib x "href");
    let childs = Xml.children x in
    List.iter (function
      | (Xml.Element _) as x -> Printf.printf "  tag=%s\n" (Xml.tag x)
      | Xml.PCData(x)        -> Printf.printf " text=%s\n" x
    ) childs;


    Printf.printf "tag=%s\n" (Xml.tag x);
    Printf.printf "href=%s\n" (Xml.attrib x "href");
    Xml.iter (fun x ->
      Printf.printf "  %s\n" (Xml.to_string x)
    ) x;

    let x = Xml.parse_file "data.xml" in
    Printf.printf("read dtd\n");

    (try
      let dtd = Dtd.parse_file "data.dtd" in
      Printf.printf("ok\n");
      let x = Dtd.prove (Dtd.check dtd) "start" x in
      print_endline (Xml.to_string x)
    with
    | Xml.Error msg as e ->
      Printf.printf "Xml error : %s\n" (Xml.error msg)
    | Dtd.Parse_error msg as e ->
      Printf.printf "Dtd parse error : %s\n" (Dtd.parse_error msg)
    | Dtd.Check_error msg as e ->
      Printf.printf "Dtd check error : %s\n" (Dtd.check_error msg)
    | Dtd.Prove_error msg as e ->
      Printf.printf "Dtd prove error : %s\n" (Dtd.prove_error msg));
  
