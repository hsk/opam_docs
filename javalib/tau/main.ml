type label = string

type instr =
  | Label of label 
  | Ldc of int 
  | Iadd 
  | Isub 
  | Imul 
  | Idiv 
  | Ifeq of label
  | Goto of label 
  | Ireturn 
  | Return 
  | Aload of int 
  | Invokenonvirtual of string
  | Iload of int 
  | Istore of int 
  | Invokestatic of string

let emit = function
  | Label l -> Printf.printf "%s:\n" l
  | Ldc i -> Printf.printf "\tldc %d\n" i
  | Iadd -> Printf.printf "\tiadd\n"
  | Isub -> Printf.printf "\tisub\n"
  | Imul -> Printf.printf "\timul\n"
  | Idiv -> Printf.printf "\tidiv\n"
  | Ifeq l -> Printf.printf "\tifeq %s\n" l
  | Goto l -> Printf.printf "\tgoto %s\n" l
  | Ireturn -> Printf.printf "\tireturn\n"
  | Return -> Printf.printf "\treturn\n"
  | Aload i when i <= 3 -> Printf.printf "\taload_%d\n" i
  | Aload i             -> Printf.printf "\taload %d\n" i
  | Invokenonvirtual m -> Printf.printf "\tinvokenonvirtual %s\n" m
  | Iload i when i <= 3 -> Printf.printf "\tiload_%d\n" i
  | Iload i             -> Printf.printf "\tiload %d\n" i
  | Istore i when i <= 3 -> Printf.printf "\tistore_%d\n" i
  | Istore i             -> Printf.printf "\tistore %d\n" i
  | Invokestatic m -> Printf.printf "\tinvokestatic Tau/%s(I)I\n" m

module Compile = struct

  open AbsTau

  type env = int ref * (string, int) Hashtbl.t

  let extend_env name (top, tbl) =
    Hashtbl.add tbl name (!top);
    incr top

  let lookup_env name (_, tbl) =
    Hashtbl.find tbl name

  let empty_env () =
    (ref 0, Hashtbl.create 10)

  let label_index = ref 0

  let new_label () =
    label_index := (!label_index + 1);
    "LABEL" ^ string_of_int (!label_index)

  let rec compileExp env = function
    | Add (exp0, exp) ->
      compileExp env exp0;
      compileExp env exp;
      emit Iadd
    | Sub (exp0, exp) ->
      compileExp env exp0;
      compileExp env exp;
      emit Isub
    | Mul (exp0, exp) ->
      compileExp env exp0;
      compileExp env exp;
      emit Imul
    | Div (exp0, exp) ->
      compileExp env exp0;
      compileExp env exp;
      emit Idiv
    | Atm(Let (VarPat (Ident name), exp0, exp)) -> 
        extend_env name env;
        compileExp env exp0;
        emit @@ Istore (lookup_env name env);
        compileExp env exp
    | Atm(Cnd (exp0, exp1, exp)) ->
      let false_label = new_label () in
      let true_label = new_label () in
      compileExp env exp0;
      emit (Ifeq false_label);
      compileExp env exp1;
      emit (Goto true_label);
      emit (Label false_label);
      compileExp env exp;
      emit (Label true_label)
    | Atm(Int n) -> emit (Ldc n)
    | Atm(Var (Ident name)) -> emit @@ Iload (lookup_env name env)
    | Atm(Exp exp) -> compileExp env exp
    | Atm(App (Ident name, exp)) ->
      compileExp env exp;
      emit @@ Invokestatic name

  let compileFunc env = function
    | Func (Ident name, VarPat (Ident param), exp) ->
        extend_env param env;
        Printf.printf ".method public static %s(I)I\n" name;
        Printf.printf ".limit locals 256\n";
        Printf.printf ".limit stack 256\n";
        compileExp env exp;
        emit Ireturn;
        Printf.printf ".end method\n"

  let compile func =
    Printf.printf ".class public Tau\n";
    Printf.printf ".super java/lang/Object\n";
    Printf.printf "\n";
    Printf.printf ".method public ()V\n";
    emit (Aload 0);
    emit (Invokenonvirtual "java/lang/Object/()V");
    emit Return;
    Printf.printf ".end method\n";
    Printf.printf "\n";
    compileFunc (empty_env ()) func
end

open Lexing

let parse (c : in_channel) : AbsTau.func =
    ParTau.pFunc LexTau.token (Lexing.from_channel c)

let _ =
    let filename = Sys.argv.(1) in
    let channel = open_in filename in
    begin try
        Compile.compile (parse channel)
    with BNFC_Util.Parse_error (start_pos, end_pos) ->
        Printf.printf "Parse error at %d.%d-%d.%d\n"
            start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
            end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
    end


