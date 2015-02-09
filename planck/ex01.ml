type e = 
  | Const of int
  | Binop of char * e * e
  | Unop of char * e

module ExpParser = struct

  open Planck

  (* Stream of chars with buffering and memoization *)
  module Stream = Schar
  module Base = Pbase.Make(Stream) (* Base parser *)
  include Base
  include Pbuffer.Extend(Stream)(Base) (* Extend Base with parser operators for buffered streams *)

  module Op = Op_prec.Make(struct
    type t = e
    type op = char
    let show_op = Printf.sprintf "(%c)"
    let app _f _a      = assert false
    let binop op a1 a2 = Binop(op, a1, a2)
    let unop op a1     = Unop(op, a1)
  end)

  let tbl = 
    let open Op_prec.Operator in
    [
      '+',  { prec = 2.0; kind = `Infix `Left };
      '-',  { prec = 2.0; kind = `Infix `Left };
      '*',  { prec = 3.0; kind = `Infix `Left };
      '/',  { prec = 3.0; kind = `Infix `Left };
      '~',  { prec = 5.0; kind = `Prefix }; (* unary minus *)
    ]

  (* parsing rules *)
  let blank = void (one_of [' '; '\t'; '\n'; '\r'])

  let rec parse s =
    let stream = Stream.from_string ~filename:"stdin" s in
    expr stream
  and simple_expr st = st |> (
    
    (* Skip spaces *)
    ?* blank >>= fun () -> (

    constant

    <|> (tokenp (function '+' | '-' | '*' | '/' | '~' -> true | _ -> false)
           >>= fun char -> return (`Op (List.assoc char tbl, char)))

    <|> (token '(' >>= fun () ->
         expr >>= fun e ->
         ?* blank >>= fun () ->
         token ')' >>= fun () -> 
         return (`Term e))
    )
  )
  and constant st = st |> (
    (* [0-9]+ *)
    matched (?+ (tokenp (function '0'..'9' -> true | _ -> false) <?> "decimal")) 
    >>= fun s -> return (`Term (Const (int_of_string s)))
  )
  and expr st = st |> (
    option (token '-') >>= fun unary_minus ->
    ?++ simple_expr >>= fun es -> 
    match unary_minus with
    | Some () -> return (Op.parse (`Op (List.assoc '~' tbl, '~') :: es))
    | None -> return (Op.parse es) 
  )

end

let rec eval = function
  | Const n -> n
  | Binop ('+', t1, t2) -> eval t1 + eval t2
  | Binop ('-', t1, t2) -> eval t1 - eval t2
  | Binop ('*', t1, t2) -> eval t1 * eval t2
  | Binop ('/', t1, t2) -> eval t1 / eval t2
  | Unop ('~', t1) -> - eval t1
  | _ -> assert false

let _ = 
  match ExpParser.parse "1 + 2 * 3" with
  | `Ok (res, _) -> Format.printf "%d@." (eval res);
  | `Error (pos, s) ->
    Format.eprintf "%a: syntax error: %s@." Planck.Position.File.format pos s
