type 'a t = 'a list
let ( >>= ) lst f = List.concat (List.map f lst)
let return x = [ x ]

(* begin *)
let _ =
  let ls = begin%monad
    x <- [10; 20; 30];
    y <- [3; 4; 5];
    return (x + y)
  end in
  (* let _ = [1; 2; 3] >>= (fun x  -> [3; 4; 5] >>= (fun y  -> return (x + y))) *)
  let ls = List.map string_of_int ls in
  let str = String.concat " " ls in
  Printf.printf "result=%s\n" str


(* fun *)
let f = fun%monad xs ys ->
  x <- xs;
  y <- ys;
  let z = x + y in
  return z

(* function *)
let rec fibm = function%monad
  | 0 -> return 0
  | 1 -> return 1
  | n ->
    x <- fibm (n - 2);
    y <- fibm (n - 1);
    return (x + y)

(* match *)
let rec fibm n = match%monad n with
  | 0 -> return 0
  | 1 -> return 1
  | _ ->
    x <- fibm (n - 2);
    y <- fibm (n - 1);
    return (x + y)

(* Toplevel let *)
let%monad f xs ys =
  let open List in
  x <- xs;
  y <- ys;
  return (x + y)

