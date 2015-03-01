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

let instrs = ref []
let labelmaps = ref []
let pos = ref 0

let emit code =
  instrs := code :: !instrs;
  match code with
  | Label(l) -> labelmaps := (l, !pos) :: !labelmaps 
  | Ifeq(_) | Goto(_) | Invokenonvirtual _ | Invokestatic _ ->
      pos := !pos + 3
  | _ ->
      pos := !pos + 1


module Emit = struct

  open Javalib_pack
  open Javalib

  let object_cn = JBasics.make_cn "java.lang.Object"

  let make_init_method cn= 
    let ms = JBasics.make_ms "<init>" [] None in
    let code = [|
      JCode.OpLoad (`Object, 0);
      JCode.OpInvoke (`Special (object_cn), ms);
      JCode.OpInvalid;
      JCode.OpInvalid;
      JCode.OpReturn `Void;
    |] in
    let jmethod = {
      JCode.c_max_stack = 1;
      c_max_locals = 1;
      c_code = code;
      c_exc_tbl = [];
      c_line_number_table = None;
      c_local_variable_table = None;
      c_local_variable_type_table = None;
      c_stack_map_midp = None;
      c_stack_map_java6 = None;
      c_attributes = []
    } in  
    let m = ConcreteMethod {
      cm_signature = ms;
      cm_class_method_signature = JBasics.make_cms cn ms;
      cm_static = false;
      cm_final = false;
      cm_synchronized = false;
      cm_strict = false;
      cm_access = `Public;
      cm_generic_signature = None;
      cm_bridge = false;
      cm_varargs = false;
      cm_synthetic = false;
      cm_other_flags = [];
      cm_exceptions = [];
      cm_attributes = { synthetic = false; deprecated = false; other = [] };
      cm_annotations = { ma_global = []; ma_parameters = [] };
      cm_implementation = Java (lazy jmethod)
    }
    in (ms, m)


  let make_main cn =
    let ms = JBasics.make_ms "main"
      [(JBasics.TObject(JBasics.TArray(JBasics.TObject(JBasics.TClass(JBasics.make_cn "java.lang.String")))))] None
    in
    let code = [|
      JCode.OpGetStatic (
       (JBasics.make_cn "java.lang.System"),
       (JBasics.make_fs "out"
                        (JBasics.TObject
                           (JBasics.TClass
                              (JBasics.make_cn "java.io.PrintStream")))));
      JCode.OpInvalid;
      JCode.OpInvalid;
      (JCode.OpConst(`Byte (10)));
      JCode.OpInvalid;
      JCode.OpInvoke (`Static cn,
       (JBasics.make_ms "f" [(JBasics.TBasic `Int)]
                        (Some (JBasics.TBasic `Int))));
      JCode.OpInvalid;
      JCode.OpInvalid;
      JCode.OpInvoke (
       `Virtual ((JBasics.TClass
                    (JBasics.make_cn "java.io.PrintStream"))),
       (JBasics.make_ms "println" [(JBasics.TBasic `Int)]
                        None));
      JCode.OpInvalid;
      JCode.OpInvalid;
      (JCode.OpReturn `Void)
    |] in
    let impl = {
      JCode.c_max_stack = 2;
      c_max_locals = 1;
      c_code = code;
      c_exc_tbl = [];
      c_line_number_table = None;
      c_local_variable_table = None;
      c_local_variable_type_table = None;
      c_stack_map_midp = None;
      c_stack_map_java6 = None;
      c_attributes = []
    } in
    let m = ConcreteMethod {
      cm_signature = ms;
      cm_class_method_signature = JBasics.make_cms cn ms;
      cm_static = true;
      cm_final = false;
      cm_synchronized = false;
      cm_strict = false;
      cm_access = `Public;
      cm_generic_signature = None;
      cm_bridge = false;
      cm_varargs = false;
      cm_synthetic = false;
      cm_other_flags = [];
      cm_exceptions = [];
      cm_attributes = { synthetic = false; deprecated = false; other = [] };
      cm_annotations = { ma_global = []; ma_parameters = [] };
      cm_implementation = Java (lazy impl)
    } in (ms, m)

  let make_method cn fname code = 
    let ms = JBasics.make_ms fname [(JBasics.TBasic `Int)] (Some (JBasics.TBasic `Int)) in
    let jmethod = {
      JCode.c_max_stack = 255;
      c_max_locals = 255;
      c_code = code;
      c_exc_tbl = [];
      c_line_number_table = None;
      c_local_variable_table = None;
      c_local_variable_type_table = None;
      c_stack_map_midp = None;
      c_stack_map_java6 = None;
      c_attributes = [];
    }
    in
    let m = ConcreteMethod {
      cm_signature = ms;
      cm_class_method_signature = JBasics.make_cms cn ms;
      cm_static = true;
      cm_final = false;
      cm_synchronized = false;
      cm_strict = false;
      cm_access = `Default;
      cm_generic_signature = None;
      cm_bridge = false;
      cm_varargs = false;
      cm_synthetic = false;
      cm_other_flags = [];
      cm_exceptions = [];
      cm_attributes = { synthetic = false; deprecated = false; other = [] };
      cm_annotations = { ma_global = []; ma_parameters = [] };
      cm_implementation = Java (lazy jmethod);
    }
    in (ms,m)


  let make_class class_name fname code =
    let src = class_name ^ ".java" in
    let cn = JBasics.make_cn class_name in

    let methods = JBasics.MethodMap.empty in
    let methods =
      let (ms, m) = make_init_method cn in
      JBasics.MethodMap.add ms m methods 
    in
    let methods =
      let (ms, m) = make_main cn in
      JBasics.MethodMap.add ms m methods 
    in

    let methods =
      let (ms, m) = make_method cn fname code in
      JBasics.MethodMap.add ms m methods
    in
    (JClass
     { c_name = cn;
       c_version = { JBasics.major = 49; minor = 0 };
       c_access = `Public;
       c_final = false;
       c_abstract = false;
       c_super_class = (Some object_cn);
       c_generic_signature = None;
       c_fields = JBasics.FieldMap.empty;
       c_interfaces = [];
       c_consts = [||];
       c_sourcefile = (Some src);
       c_deprecated = false;
       c_enclosing_method = None;
       c_source_debug_extention = None;
       c_inner_classes = [];
       c_synthetic = false;
       c_enum = false;
       c_annotations = [];
       c_other_flags = [];
       c_other_attributes = [];
       c_methods = methods;
      })

  let (codes:JCode.jopcode array ref)= ref [||]
  let pos = ref 0

  let f cname = function
  | Label(_) -> ()
  | Ldc(i) -> !codes.(!pos) <- JCode.OpConst(`Int (Int32.of_int i)); pos := !pos + 1
  | Iadd -> !codes.(!pos) <- (JCode.OpAdd `Int2Bool); pos := !pos + 1
  | Isub -> !codes.(!pos) <- (JCode.OpSub `Int2Bool); pos := !pos + 1
  | Imul -> !codes.(!pos) <- (JCode.OpMult `Int2Bool); pos := !pos + 1
  | Idiv -> !codes.(!pos) <- (JCode.OpDiv `Int2Bool); pos := !pos + 1
  | Ifeq(l) -> !codes.(!pos) <- JCode.OpIf (`Eq, (List.assoc l !labelmaps)- !pos); pos := !pos + 3
  | Goto(l) -> !codes.(!pos) <- JCode.OpGoto((List.assoc l !labelmaps)- !pos); pos := !pos + 3
  | Ireturn -> !codes.(!pos) <- JCode.OpReturn `Int2Bool; pos := !pos + 1
  | Return -> !codes.(!pos) <- JCode.OpReturn `Void; pos := !pos + 1
  | Aload(i)  -> !codes.(!pos) <- JCode.OpConst(`Int (Int32.of_int i)); pos := !pos + 1
  | Iload(i)  -> !codes.(!pos) <- JCode.OpLoad (`Int2Bool, i); pos := !pos + 1
  | Istore(i) -> !codes.(!pos) <- JCode.OpStore(`Int2Bool, i); pos := !pos + 1
  | Invokenonvirtual(x) ->
    !codes.(!pos) <-
      JCode.OpInvoke (`Virtual (JBasics.TClass(JBasics.make_cn cname)),
        (JBasics.make_ms x [(JBasics.TBasic `Int)]
        (Some (JBasics.TBasic `Int))));
    pos := !pos + 3

  | Invokestatic(x) ->
    !codes.(!pos) <-
      JCode.OpInvoke (`Static ((JBasics.make_cn cname)),
        (JBasics.make_ms x [(JBasics.TBasic `Int)]
        (Some (JBasics.TBasic `Int))));
    pos := !pos + 3

  let emit cname name p =
    pos := 0;
    codes := Array.create p JCode.OpInvalid;
    List.iter (f cname) (List.rev !instrs);
    let k = make_class "Tau" name !codes in
    Javalib.unparse_class k (open_out "Tau.class")

end

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
        compileExp env exp;
        emit Ireturn;
        Emit.emit "Tau" name !pos

  let compile func =
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


