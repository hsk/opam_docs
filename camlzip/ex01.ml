let compress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.compress (fun buf -> input ic buf 0 (String.length buf))
                (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let uncompress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.uncompress (fun buf -> input ic buf 0 (String.length buf))
                  (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let _ =
  let oc = open_out "test.txt" in
  Printf.fprintf oc "test\n";
  close_out oc;

  compress "test.txt" "test.dat";
  uncompress "test.dat" "test2.txt"


