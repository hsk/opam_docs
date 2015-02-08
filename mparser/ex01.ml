open MParser

let infix p o =
  Infix (p |>> (fun _ a b -> (`Binop (o, a, b))), Assoc_left)

let operators =
  [ [ infix (char '*') `Mul;
      infix (char '/') `Div ];
    [ infix (char '+') `Add;
      infix (char '-') `Sub ] ]

let expr =
  expression operators (Tokens.decimal |>> fun i -> `Int i)

let rec calc = function
  | `Int i -> i
  | `Binop (op, a, b) ->
      match op with
        | `Add -> calc a + calc b
        | `Sub -> calc a - calc b
        | `Mul -> calc a * calc b
        | `Div -> calc a / calc b

let eval (s: string) : int =
  match MParser.parse_string expr s () with
    | Success e -> calc e
    | Failed (msg, e) -> failwith msg

let _ =
  Printf.printf "%d\n" (eval "4 *4+10/2")

