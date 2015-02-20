let _ =

  Printf.printf "list to csv\n";
  let csv = [["aaa";"bbb";"ccc"];["kkk"]] in
  Csv.print_readable csv;
  Printf.printf "------\n";

  Printf.printf "save and load\n";
  Csv.save "a.csv" csv;
  let csv = Csv.load "a.csv" in
  Csv.print_readable csv;
  Printf.printf "------\n";

  Printf.printf "read string\n";
  let csv = Csv.input_all(Csv.of_string "csv,data,aaa") in
  Csv.print_readable csv
