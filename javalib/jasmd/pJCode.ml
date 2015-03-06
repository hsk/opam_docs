open Javalib_pack
open JBasics
open JCode
let pp_i fmt i =
  if i < 0
  then Format.fprintf fmt "(%d)" i
  else Format.fprintf fmt "%d" i

let rec pp_jconst fmt = function
  | `ANull -> Format.pp_print_string fmt "`ANull"
  | `Int x -> Format.fprintf fmt "`Int (@[<hov>%ldl@])" x
  | `Long x -> Format.fprintf fmt "`Long (@[<hov>%LdL@])" x
  | `Float x -> Format.fprintf fmt "`Float (@[<hov>%F@])" x
  | `Double x -> Format.fprintf fmt "`Double (@[<hov>%F@])" x
  | `Byte x -> Format.fprintf fmt "`Byte (@[<hov>%d@])" x
  | `Short x -> Format.fprintf fmt "`Short (@[<hov>%d@])" x
  | `String x -> Format.fprintf fmt "`String (@[<hov>%a@])" PJBasics.pp_jstr x
  | `Class x -> Format.fprintf fmt "`Class (@[<hov>%a@])" PJBasics.pp_object_type x

and show_jconst x = Format.asprintf "%a" pp_jconst x

let rec pp_jopcode fmt = function
  | OpLoad (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpLoad (@,%a,@ %d@])" PJBasics.pp_jvm_type a0 a1
  | OpStore (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpStore (@,%a,@ %d@])" PJBasics.pp_jvm_type a0 a1;
  | OpIInc (a0,a1) -> Format.fprintf fmt "@[<hov2>JCode.OpIInc (@,%d,@ %d@])" a0 a1;
  | OpPop -> Format.pp_print_string fmt "JCode.OpPop"
  | OpPop2 -> Format.pp_print_string fmt "JCode.OpPop2"
  | OpDup -> Format.pp_print_string fmt "JCode.OpDup"
  | OpDupX1 -> Format.pp_print_string fmt "JCode.OpDupX1"
  | OpDupX2 -> Format.pp_print_string fmt "JCode.OpDupX2"
  | OpDup2 -> Format.pp_print_string fmt "JCode.OpDup2"
  | OpDup2X1 -> Format.pp_print_string fmt "JCode.OpDup2X1"
  | OpDup2X2 -> Format.pp_print_string fmt "JCode.OpDup2X2"
  | OpSwap -> Format.pp_print_string fmt "JCode.OpSwap"
  | OpConst a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpConst(@,%a)@])" pp_jconst a0
  | OpAdd a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpAdd@ %a@])" PJBasics.pp_jvm_basic_type a0
  | OpSub a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpSub@ %a@])" PJBasics.pp_jvm_basic_type a0
  | OpMult a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpMult@ %a@])" PJBasics.pp_jvm_basic_type a0
  | OpDiv a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpDiv@ %a@])" PJBasics.pp_jvm_basic_type a0
  | OpRem a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpRem@ %a@])" PJBasics.pp_jvm_basic_type a0
  | OpNeg a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpNeg@ %a@])" PJBasics.pp_jvm_basic_type a0
  | OpIShl -> Format.pp_print_string fmt "JCode.OpIShl"
  | OpLShl -> Format.pp_print_string fmt "JCode.OpLShl"
  | OpIShr -> Format.pp_print_string fmt "JCode.OpIShr"
  | OpLShr -> Format.pp_print_string fmt "JCode.OpLShr"
  | OpIUShr -> Format.pp_print_string fmt "JCode.OpIUShr"
  | OpLUShr -> Format.pp_print_string fmt "JCode.OpLUShr"
  | OpIAnd -> Format.pp_print_string fmt "JCode.OpIAnd"
  | OpLAnd -> Format.pp_print_string fmt "JCode.OpLAnd"
  | OpIOr -> Format.pp_print_string fmt "JCode.OpIOr"
  | OpLOr -> Format.pp_print_string fmt "JCode.OpLOr"
  | OpIXor -> Format.pp_print_string fmt "JCode.OpIXor"
  | OpLXor -> Format.pp_print_string fmt "JCode.OpLXor"
  | OpI2L -> Format.pp_print_string fmt "JCode.OpI2L"
  | OpI2F -> Format.pp_print_string fmt "JCode.OpI2F"
  | OpI2D -> Format.pp_print_string fmt "JCode.OpI2D"
  | OpL2I -> Format.pp_print_string fmt "JCode.OpL2I"
  | OpL2F -> Format.pp_print_string fmt "JCode.OpL2F"
  | OpL2D -> Format.pp_print_string fmt "JCode.OpL2D"
  | OpF2I -> Format.pp_print_string fmt "JCode.OpF2I"
  | OpF2L -> Format.pp_print_string fmt "JCode.OpF2L"
  | OpF2D -> Format.pp_print_string fmt "JCode.OpF2D"
  | OpD2I -> Format.pp_print_string fmt "JCode.OpD2I"
  | OpD2L -> Format.pp_print_string fmt "JCode.OpD2L"
  | OpD2F -> Format.pp_print_string fmt "JCode.OpD2F"
  | OpI2B -> Format.pp_print_string fmt "JCode.OpI2B"
  | OpI2C -> Format.pp_print_string fmt "JCode.OpI2C"
  | OpI2S -> Format.pp_print_string fmt "JCode.OpI2S"
  | OpCmp a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpCmp@ ";
      begin match a0 with
      | `L -> Format.pp_print_string fmt "`L"
      | `FL -> Format.pp_print_string fmt "`FL"
      | `FG -> Format.pp_print_string fmt "`FG"
      | `DL -> Format.pp_print_string fmt "`DL"
      | `DG -> Format.pp_print_string fmt "`DG"
      end;
      Format.fprintf fmt "@])"
  | OpIf (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpIf (@,";
      begin match a0 with
      | `Eq -> Format.pp_print_string fmt "`Eq"
      | `Ne -> Format.pp_print_string fmt "`Ne"
      | `Lt -> Format.pp_print_string fmt "`Lt"
      | `Ge -> Format.pp_print_string fmt "`Ge"
      | `Gt -> Format.pp_print_string fmt "`Gt"
      | `Le -> Format.pp_print_string fmt "`Le"
      | `Null -> Format.pp_print_string fmt "`Null"
      | `NonNull -> Format.pp_print_string fmt "`NonNull"
      end;
      Format.fprintf fmt ",@ %d@])" a1
  | OpIfCmp (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpIfCmp (@,";
      begin match a0 with
      | `IEq -> Format.pp_print_string fmt "`IEq"
      | `INe -> Format.pp_print_string fmt "`INe"
      | `ILt -> Format.pp_print_string fmt "`ILt"
      | `IGe -> Format.pp_print_string fmt "`IGe"
      | `IGt -> Format.pp_print_string fmt "`IGt"
      | `ILe -> Format.pp_print_string fmt "`ILe"
      | `AEq -> Format.pp_print_string fmt "`AEq"
      | `ANe -> Format.pp_print_string fmt "`ANe"
      end;
      Format.fprintf fmt ",@ %d@])" a1
  | OpGoto a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpGoto@ %a@])" pp_i a0
  | OpJsr a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpJsr@ %a@])" pp_i a0
  | OpRet a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpRet@ %a@])" pp_i a0
  | OpTableSwitch (a0,a1,a2,a3) ->
      Format.fprintf fmt "@[<hov2>JCode.OpTableSwitch (@,";
      begin
        Format.fprintf fmt "%d,@ %ldl,@ %ldl,@ " a0 a1 a2;
        begin
          Format.fprintf fmt "[|@[<hov>";
          ignore begin Array.fold_left
            (fun sep x ->
                if sep then Format.fprintf fmt ";@ ";
                Format.fprintf fmt "%d" x;
                true
            ) false a3
          end;
          Format.fprintf fmt "@]|]"
        end
      end;
      Format.fprintf fmt "@])"
  | OpLookupSwitch (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpLookupSwitch (@,%d,@ " a0;
      begin
        Format.fprintf fmt "[@[<hov>";
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%ldl,@ %d@])" a0 a1;
            true
          ) false a1
        end;
        Format.fprintf fmt "@]]"
      end;
      Format.fprintf fmt "@])"

  | OpNew a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpNew@ %a@])" PJBasics.pp_class_name a0
  | OpNewArray a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpNewArray@ %a@])" PJBasics.pp_value_type a0
  | OpAMultiNewArray (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpAMultiNewArray (@,%a,@ %d@])" PJBasics.pp_object_type a0 a1
  | OpCheckCast a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpCheckCast@ %a@])" PJBasics.pp_object_type a0
  | OpInstanceOf a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpInstanceOf@ %a@])" PJBasics.pp_object_type a0
  | OpGetStatic (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpGetStatic (@,%a,@ %a@])"
        PJBasics.pp_class_name a0 PJBasics.pp_field_signature a1
  | OpPutStatic (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpPutStatic (@,%a,@ %a@])"
        PJBasics.pp_class_name a0 PJBasics.pp_field_signature a1
  | OpGetField (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpGetField (@,%a,@ %a@])"
        PJBasics.pp_class_name a0 PJBasics.pp_field_signature a1
  | OpPutField (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpPutField (@,%a,@ %a@])"
        PJBasics.pp_class_name a0 PJBasics.pp_field_signature a1
  | OpArrayLength -> Format.pp_print_string fmt "JCode.OpArrayLength"
  | OpArrayLoad a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpArrayLoad@ %a@])" PJBasics.pp_jvm_array_type a0
  | OpArrayStore a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpArrayStore@ %a@])" PJBasics.pp_jvm_array_type a0
  | OpInvoke (a0,a1) ->
      Format.fprintf fmt "@[<hov2>JCode.OpInvoke (@,";
      begin match a0 with
      | `Virtual x -> Format.fprintf fmt "`Virtual (@[<hov>%a@])" PJBasics.pp_object_type x
      | `Special x -> Format.fprintf fmt "`Special (@[<hov>%a@])" PJBasics.pp_class_name x
      | `Static x -> Format.fprintf fmt "`Static (@[<hov>%a@])" PJBasics.pp_class_name x
      | `Interface x -> Format.fprintf fmt "`Interface (@[<hov>%a@])" PJBasics.pp_class_name x
      end;
      Format.fprintf fmt ",@ %a@])" PJBasics.pp_method_signature a1
  | OpReturn a0 ->
      Format.fprintf fmt "(@[<hov2>JCode.OpReturn@ %a@])" PJBasics.pp_jvm_return_type a0
  | OpThrow -> Format.pp_print_string fmt "JCode.OpThrow"
  | OpMonitorEnter -> Format.pp_print_string fmt "JCode.OpMonitorEnter"
  | OpMonitorExit -> Format.pp_print_string fmt "JCode.OpMonitorExit"
  | OpNop -> Format.pp_print_string fmt "JCode.OpNop"
  | OpBreakpoint -> Format.pp_print_string fmt "JCode.OpBreakpoint"
  | OpInvalid -> Format.pp_print_string fmt "JCode.OpInvalid"

and show_jopcode x = Format.asprintf "%a" pp_jopcode x

let rec pp_jopcodes fmt x =
  Format.fprintf fmt "[|@[<hov>";
  begin
    ignore begin Array.fold_left
      (fun sep x ->
        if sep then Format.fprintf fmt ";@\n";
        pp_jopcode fmt x;
        true
      ) false x 
    end;
  end;
  Format.fprintf fmt "@]|]"

and show_jopcodes x = Format.asprintf "%a" pp_jopcodes x

let rec pp_exception_handler fmt x =
  Format.fprintf fmt "{ @[<hov>";
  begin
    Format.fprintf fmt "JCode.e_start = %d;@\n" x.e_start;
    Format.fprintf fmt "e_end = %d;@\n" x.e_end;
    Format.fprintf fmt "e_handler = %d;@\n" x.e_handler;
    Format.pp_print_string fmt "e_catch_type = ";
    begin
      match x.e_catch_type with
      | None -> Format.pp_print_string fmt "None"
      | Some x -> Format.fprintf fmt "(Some %a)" PJBasics.pp_class_name x
    end;
  end;
  Format.fprintf fmt "@] }"

and show_exception_handler x = Format.asprintf "%a" pp_exception_handler x

let rec pp_jcode fmt x =
  Format.fprintf fmt "{ @[<hov>";
  begin
    Format.fprintf fmt "JCode.c_max_stack = %d;@\n" x.c_max_stack;
    Format.fprintf fmt "c_max_locals = %d;@\n" x.c_max_locals;
    Format.fprintf fmt "c_code =@\n  @[%a;@]@\n" pp_jopcodes x.c_code;
    Format.fprintf fmt "c_exc_tbl = [@[<hov>";
    begin
      ignore begin List.fold_left
        (fun sep x ->
          if sep then Format.fprintf fmt ";@\n";
          pp_exception_handler fmt x;
          true
        ) false x.c_exc_tbl
      end;
      Format.fprintf fmt "@]];@\n"
    end;
    Format.fprintf fmt "c_line_number_table = ";
    begin
      begin match x.c_line_number_table with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.fprintf fmt "(Some [@[<hov>";
          ignore begin List.fold_left
            (fun sep (a0,a1) ->
              if sep then Format.fprintf fmt ";@\n";
              Format.fprintf fmt "(@[<hov>%d,@ %d@])" a0 a1;
              true
            ) false x
          end;
          Format.fprintf fmt "@]])"
      end;
      Format.fprintf fmt ";@\n"
    end;
    Format.fprintf fmt "c_local_variable_table = ";
    begin
      begin match x.c_local_variable_table with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.fprintf fmt "(Some [@[<hov>";
          ignore begin List.fold_left
            (fun sep (a0,a1,a2,a3,a4) ->
              if sep then Format.fprintf fmt ";@\n";
              Format.fprintf fmt "(@[<hov>%d,@ %d,@ %S,@ %a,@ %d@])"
                a0 a1 a2 PJBasics.pp_value_type a3 a4;
              true
            ) false x
          end;
          Format.fprintf fmt "@]]";
          Format.pp_print_string fmt ")"
      end;
      Format.fprintf fmt ";@\n"
    end;
    Format.fprintf fmt "c_local_variable_type_table = ";
    begin
      begin match x.c_local_variable_type_table with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.fprintf fmt "(Some [@[<hov>";
          ignore begin List.fold_left
            (fun sep (a0,a1,a2,a3,a4) ->
              if sep then Format.fprintf fmt ";@\n";
              Format.fprintf fmt "(@[<hov>%d,@ %d,@ %S,@ %a,@ %d@])"
                a0 a1 a2 PJSignature.pp_fieldTypeSignature a3 a4;
              true
            ) false x 
          end;
          Format.fprintf fmt "@]])"
      end;
      Format.fprintf fmt ";@\n"
    end;
    Format.fprintf fmt "c_stack_map_midp = ";
    begin
      begin match x.c_stack_map_midp with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.fprintf fmt "(Some [@[<hov>";
          ignore begin List.fold_left
            (fun sep x ->
              if sep then Format.fprintf fmt ";@\n";
              PJBasics.pp_stackmap fmt x;
              true
            ) false x
          end;
          Format.fprintf fmt "@]])"
      end;
      Format.fprintf fmt ";@\n"
    end;
    Format.fprintf fmt "c_stack_map_java6 = ";
    begin
      begin match x.c_stack_map_java6 with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
          Format.fprintf fmt "(Some [@[<hov>";
          ignore begin List.fold_left
            (fun sep x ->
              if sep then Format.fprintf fmt ";@\n";
              PJBasics.pp_stackmap fmt x;
              true
            ) false x
          end;
          Format.fprintf fmt "@]])"
      end;
      Format.fprintf fmt ";@\n"
    end;
    Format.fprintf fmt "c_attributes = [@[<hov>";
    begin
      ignore begin List.fold_left
        (fun sep (a0,a1) ->
          if sep then Format.fprintf fmt ";@\n";
          Format.fprintf fmt "(@[<hov>%S,@ %S@])" a0 a1;
          true
        ) false x.c_attributes
      end;
      Format.fprintf fmt "@]]"
    end;
  end;
  Format.fprintf fmt "@] }"

and show_jcode x = Format.asprintf "%a" pp_jcode x

let pp = pp_jcode
let show = show_jcode
