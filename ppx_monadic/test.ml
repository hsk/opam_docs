module Option = struct
  let bind e f = match e with
    | Some v -> f v
    | None -> None
  let return v = Some v
end

let t1 = 
  let open Option in do_
  ; x <-- return 1
  ; return x


