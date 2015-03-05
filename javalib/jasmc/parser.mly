%{
open Javalib_pack
open Javalib
type sis = (string * int) list
[@@deriving show]

module PJBasics = struct
  open Javalib_pack
  open JBasics

  let rec pp_class_name fmt nam =
     Format.fprintf fmt "(JBasics.make_cn %S)" (JPrint.class_name nam)

  and show_class_name x = Format.asprintf "%a" pp_class_name x

  let rec pp_other_num fmt = function
    | `Long -> Format.pp_print_string fmt "`Long"
    | `Float -> Format.pp_print_string fmt "`Float"
    | `Double -> Format.pp_print_string fmt "`Double"

  and show_other_num x = Format.asprintf "%a" pp_other_num x

  let rec pp_jvm_basic_type fmt = function
    | `Int2Bool -> Format.pp_print_string fmt "`Int2Bool"
    | #other_num as x -> pp_other_num fmt x

  and show_jvm_basic_type x = Format.asprintf "%a" pp_jvm_basic_type x

  let rec pp_jvm_type fmt = function
    | #jvm_basic_type as x -> pp_jvm_basic_type fmt x
    | `Object -> Format.pp_print_string fmt "`Object"

  and show_jvm_type x = Format.asprintf "%a" pp_jvm_type x

  let rec pp_jvm_array_type fmt = function
    | `Int -> Format.pp_print_string fmt "`Int"
    | `Short -> Format.pp_print_string fmt "`Short"
    | `Char -> Format.pp_print_string fmt "`Char"
    | `ByteBool -> Format.pp_print_string fmt "`ByteBool"
    | #other_num as x -> pp_other_num fmt x
    | `Object -> Format.pp_print_string fmt "`Object"

  and show_jvm_array_type x = Format.asprintf "%a" pp_jvm_array_type x

  let rec pp_jvm_return_type fmt = function
    | #jvm_basic_type as x -> pp_jvm_basic_type fmt x
    | `Object -> Format.pp_print_string fmt "`Object"
    | `Void -> Format.pp_print_string fmt "`Void"

  and show_jvm_return_type x = Format.asprintf "%a" pp_jvm_return_type x

  let rec pp_java_basic_type fmt = function
    | `Int -> Format.pp_print_string fmt "`Int"
    | `Short -> Format.pp_print_string fmt "`Short"
    | `Char -> Format.pp_print_string fmt "`Char"
    | `Byte -> Format.pp_print_string fmt "`Byte"
    | `Bool -> Format.pp_print_string fmt "`Bool"
    | #other_num as x -> pp_other_num fmt x

  and show_java_basic_type x = Format.asprintf "%a" pp_java_basic_type x

  let rec pp_object_type fmt = function
    | TClass a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.TClass@ %a@])" pp_class_name a0
    | TArray a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.TArray@ %a@])" pp_value_type a0

  and show_object_type x = Format.asprintf "%a" pp_object_type x

  and pp_value_type fmt = function
    | TBasic a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.TBasic@ %a@])" pp_java_basic_type a0
    | TObject a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.TObject@ %a@])" pp_object_type a0

  and show_value_type x = Format.asprintf "%a" pp_value_type x

  let rec pp_method_descriptor fmt (a0,a1) =
    Format.fprintf fmt "(@[<hov>";
    begin
      begin
        Format.fprintf fmt "[@[<hov>";
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_value_type fmt x;
            true
          ) false a0
        end;
        Format.fprintf fmt "@]],@ "
      end;
      begin match a1 with
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" pp_value_type x
      end
    end;
    Format.fprintf fmt "@])"

  and show_method_descriptor x = Format.asprintf "%a" pp_method_descriptor x

  let rec pp_method_signature_data fmt (a0,a1) =
    Format.fprintf fmt "(@[<hov>%S,@ %a@])" a0 pp_method_descriptor a1

  and show_method_signature_data x = Format.asprintf "%a" pp_method_signature_data x

  let rec pp_method_signature fmt ms =
    Format.fprintf fmt "(JBasics.make_ms @[<hov>%S@ " (JBasics.ms_name ms);
    begin
      begin
        Format.fprintf fmt "[@[<hov>";
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_value_type fmt x;
            true
          ) false (JBasics.ms_args ms)
        end;
        Format.fprintf fmt "@]]@ "
      end;
      begin
        match JBasics.ms_rtype ms with
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" pp_value_type x
      end
    end;
    Format.fprintf fmt "@])"

  and show_method_signature x = Format.asprintf "%a" pp_method_signature x

  let rec pp_field_signature fmt fs =
    Format.fprintf fmt "(JBasics.make_fs @[<hov>%S@ %a@])"
      (JBasics.fs_name fs) pp_value_type (JBasics.fs_type fs)

  and show_field_signature x = Format.asprintf "%a" pp_field_signature x

  let rec pp_class_field_signature_data fmt (a0,a1) =
    Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_class_name a0 pp_field_signature a1

  and show_class_field_signature_data x = Format.asprintf "%a" pp_class_field_signature_data x
  
  let rec pp_class_method_signature_data fmt (a0,a1) =
    Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_class_name a0 pp_method_signature a1

  and show_class_method_signature_data x = Format.asprintf "%a" pp_class_method_signature_data x

  let rec pp_class_field_signature fmt cfs =
    let (name, fs) = cfs_split cfs in
    Format.fprintf fmt "(JBasics.make_cfs @[<hov>%a@ %a@])" pp_class_name name pp_field_signature fs

  and show_class_field_signature x = Format.asprintf "%a" pp_class_field_signature x

  let rec pp_class_method_signature fmt cms =
    let (name, ms) = cms_split cms in
    Format.fprintf fmt "(JBasics.make_cms @[<hov>%a@ %a@])" pp_class_name name pp_method_signature ms

  and show_class_method_signature x = Format.asprintf "%a" pp_class_method_signature x

  let rec pp_descriptor fmt = function
    | SValue a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.SValue@ %a@])" pp_value_type a0
    | SMethod a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.SMethod@ %a@])" pp_method_descriptor a0

  and show_descriptor x = Format.asprintf "%a" pp_descriptor x

  let rec pp_jstr fmt str = Format.fprintf fmt "%S" (jstr_raw str)

  and show_jstr x = Format.asprintf "%a" pp_jstr x

  let rec pp_constant_value fmt = function
    | ConstString a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstString@ %a@])" pp_jstr a0
    | ConstInt a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstInt@ %ldl@])" a0
    | ConstFloat a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstFloat@ %F@])" a0
    | ConstLong a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstLong@ %LdL@])" a0
    | ConstDouble a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstDouble@ %F@])" a0
    | ConstClass a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstClass@ %a@])" pp_object_type a0;

  and show_constant_value x = Format.asprintf "%a" pp_constant_value x

  let rec pp_constant fmt = function
    | ConstValue a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.ConstValue@ %a@])" pp_constant_value a0
    | ConstField (a0,a1) ->
        Format.fprintf fmt "(@[<hov2>JBasics.ConstField@ ";
        Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_class_name a0 pp_field_signature a1;
        Format.fprintf fmt "@])"
    | ConstMethod (a0,a1) ->
        Format.fprintf fmt "(@[<hov2>JBasics.ConstMethod@ ";
        Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_object_type a0 pp_method_signature a1;
        Format.fprintf fmt "@])"
    | ConstInterfaceMethod (a0,a1) ->
        Format.fprintf fmt "(@[<hov2>JBasics.ConstInterfaceMethod@ ";
        Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_class_name a0 pp_method_signature a1;
        Format.fprintf fmt "@])"
    | ConstNameAndType (a0,a1) ->
        Format.fprintf fmt "@[<hov2>JBasics.ConstNameAndType (@,%S,@ %a@])"
          a0 pp_descriptor a1;
    | ConstStringUTF8 a0 -> Format.fprintf fmt "(@[<hov2>JBasics.ConstStringUTF8@ %S@])" a0
    | ConstUnusable -> Format.pp_print_string fmt "JBasics.ConstUnusable"

  and show_constant x = Format.asprintf "%a" pp_constant x

  let rec pp_verification_type fmt = function
    | VTop -> Format.pp_print_string fmt "JBasics.VTop"
    | VInteger -> Format.pp_print_string fmt "JBasics.VInteger"
    | VFloat -> Format.pp_print_string fmt "JBasics.VFloat"
    | VDouble -> Format.pp_print_string fmt "JBasics.VDouble"
    | VLong -> Format.pp_print_string fmt "JBasics.VLong"
    | VNull -> Format.pp_print_string fmt "JBasics.VNull"
    | VUninitializedThis -> Format.pp_print_string fmt "JBasics.VUninitializedThis"
    | VObject a0 -> Format.fprintf fmt "(@[<hov2>JBasics.VObject@ %a@])" pp_object_type a0
    | VUninitialized a0 -> Format.fprintf fmt "(@[<hov2>JBasics.VUninitialized@ %d@])" a0

  and show_verification_type x = Format.asprintf "%a" pp_verification_type x
  
  let rec pp_stackmap fmt (a0,a1,a2) =
    Format.fprintf fmt "(@[<hov>";
    begin
      Format.fprintf fmt "%d,@ " a0;
      begin
        Format.fprintf fmt "[@[<hov>";
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_verification_type fmt x;
            true
          ) false a1
        end;
        Format.fprintf fmt "@]],@ "
      end;
      begin
        Format.fprintf fmt "[@[<hov>";
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_verification_type fmt x;
            true
          ) false a2
        end;
        Format.fprintf fmt "@]]"
      end;
    end;
    Format.fprintf fmt "@])"

  and show_stackmap x = Format.asprintf "%a" pp_stackmap x

  let rec pp_version fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JBasics.major = %d;@ " x.major;
      Format.fprintf fmt "minor = %d" x.minor;
    end;
    Format.fprintf fmt "@] }"

  and show_version x = Format.asprintf "%a" pp_version x

  let rec pp_element_value fmt =
    function
    | EVCstByte a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstByte@ %d@])" a0;
    | EVCstChar a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstChar@ %d@])" a0;
    | EVCstInt a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstInt@ %ldl@])" a0;
    | EVCstShort a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstShort@ %d@])" a0;
    | EVCstBoolean a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstBoolean@ %d@])" a0;
    | EVCstDouble a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstDouble@ %F@])" a0;
    | EVCstFloat a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstFloat@ %F@])" a0;
    | EVCstLong a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstLong@ %LdL@])" a0;
    | EVCstString a0 -> Format.fprintf fmt "(@[<hov2>JBasics.EVCstString@ %S@])" a0;
    | EVEnum (a0,a1) ->
        Format.fprintf fmt "(@[<hov2>JBasics.EVEnum@ ";
        Format.fprintf fmt "(@[<hov>%a,@ %S@])" pp_class_name a0 a1;
        Format.fprintf fmt "@])"
    | EVClass a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.EVClass@ ";
        begin
          begin match a0 with
            | None -> Format.pp_print_string fmt "None"
            | Some x -> Format.fprintf fmt "(Some %a)" pp_value_type x
          end
        end;
        Format.fprintf fmt "@])"
    | EVAnnotation a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.EVAnnotation@ %a@])"  pp_annotation a0
    | EVArray a0 ->
        Format.fprintf fmt "(@[<hov2>JBasics.EVArray@ ";
        begin
          begin
            Format.fprintf fmt "[@[<hov>";
            ignore begin List.fold_left
              (fun sep x ->
                if sep then Format.fprintf fmt ";@ ";
                pp_element_value fmt x;
                true
              ) false a0
            end;
            Format.fprintf fmt "@]]"
          end
        end;
        Format.fprintf fmt "@])"

  and show_element_value x = Format.asprintf "%a" pp_element_value x

  and pp_annotation fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JBasics.kind = %a;@ " pp_class_name x.kind;
      Format.fprintf fmt "element_value_pairs = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0, a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%S,@ %a@])" a0 pp_element_value a1;
            true
          ) false x.element_value_pairs
        end;
        Format.fprintf fmt "@]]"
      end
    end;
    Format.fprintf fmt "@] }"

  and show_annotation x = Format.asprintf "%a" pp_annotation x

end

let sourcefile = ref None

let replace_dot s =
  let s = String.copy s in
    for i = 0 to String.length s - 1 do
      if s.[i] = '/' then s.[i] <- '.'
    done;
    s
let split_method name =
  let pos = String.index name '(' in
  (String.sub name 0 pos, String.sub name pos ((String.length name) - pos))

let split_obj name =
  let name = replace_dot name in
  let pos = String.rindex name '.' in
  (String.sub name 0 pos, String.sub name (pos+1) ((String.length name) - (pos+1)))

let cn = ref (JBasics.make_cn "_")

let cbasic = function 
    | "boolean" -> "Z"
    | "byte"    -> "B"
    | "char"    -> "C"
    | "short"   -> "S"
    | "int"     -> "I"
    | "long"    -> "J"
    | "float"   -> "F"
    | "double"  -> "D"
    | "void"    -> "V"
    | _ -> assert false

let cf_access a =
  if List.mem `Public a then `Public else
  if List.mem `Protected a then `Protected else
  if List.mem `Private a then `Private else
  `Default

type switchcase = 
| CaseIntWord of int * string
| CaseIntInt of int * int

type switchdefault =
| DefaultWord of string
| DefaultInt of int

type pcode =
| DLine of int
| Label of string
| LabelInst of string * pcode
| Invalid
| Inst of (string * string)
| InstIntInt of (string * string) * int * int
| InstInt of (string * string) * int
| InstNum of (string * string) * string
| InstWord of (string * string) * string
| InstWordInt of (string * string) * string * int
| InstWordWord of (string * string) * string * string
| InstStr of (string * string) * string
| InstRelative of (string * string) * string
| LookupSwitch of switchcase list * switchdefault
| TableSwitch of (int * int) * switchdefault list * switchdefault

let limit_stack = ref 0
let limit_locals = ref 0

let mkcode ss =
  let pos = ref 0 in
  
  let labels = ref [] in
  let label2int l =
    (*Format.printf "label2int %s %d\n" l (List.length !labels);*)
    let v = if (List.mem_assoc l !labels) then List.assoc l !labels else 0 in
    (*Format.printf "v=%d\n" v;*)
    if v <> 0 then v else
    let pos = -(List.length !labels) - 1 in
    labels := !labels @ [l, pos];
    (*Format.printf "pos=%d\n" pos;*)
    
    pos
  in
  let addlabel l i =
    Format.printf "addlabel %d %a\n" (List.length !labels) pp_sis !labels;
    let rec f = function
    | [] -> [l,i]
    | (x,v)::xs when x = l -> (l,i)::xs
    | x::xs -> x:: f xs
    in labels := f !labels;
    Format.printf "result %a\n" pp_sis !labels;

  in
  let realloc p l =
    Format.printf "realloc %d %d\n" p l;
    if l >= 0 then l else
    match List.nth !labels (-l-1)  with
    | (l,v) -> v - p
  in

  let add n c =
    let p = !pos in
    pos := !pos + n;
    (p, c)
  in
  let unstr s =
    String.sub s 1 ((String.length s)- 2)
  in
  let rec f c = 
    match c with
    | DLine i -> add 0 JCode.OpInvalid
    | Label l -> addlabel l !pos; add 0 JCode.OpInvalid
    | LabelInst(l,i)->
      addlabel l !pos; f i
    | Invalid -> add 0 JCode.OpInvalid

    (* A *)
    | Inst("aaload", "") -> add 1 (JCode.OpArrayLoad `Object)
    | Inst("aastore", "") -> add 1 (JCode.OpArrayStore `Object)
    | Inst("aconst_null", "") -> add 1 (JCode.OpConst(`ANull))
    | InstInt(("aload", "i"), n) -> add 2 (JCode.OpConst(`Int (Int32.of_int n)))
    | InstInt(("aload", "I"), n) -> add 3 (JCode.OpConst(`Int (Int32.of_int n)))
    | Inst("aload_0", "") -> add 1 (JCode.OpLoad (`Object, 0))
    | Inst("aload_1", "") -> add 1 (JCode.OpLoad (`Object, 1))
    | Inst("aload_2", "") -> add 1 (JCode.OpLoad (`Object, 2))
    | Inst("aload_3", "") -> add 1 (JCode.OpLoad (`Object, 3))
    | InstWord(("anewarray", "class"), o) ->
      let a = JParseSignature.parse_objectType o in
      add 3 (JCode.OpNewArray (JBasics.TObject a))
    | Inst("areturn", "") -> add 1 (JCode.OpReturn `Object)
    | Inst("arraylength", "") -> add 1 (JCode.OpArrayLength)
    | InstInt(("astore", "i"), n) -> add 2 (JCode.OpStore(`Object, n))
    | InstInt(("astore", "I"), n) -> add 3 (JCode.OpStore(`Object, n))
    | Inst("astore_0","") -> add 1 (JCode.OpStore (`Object, 0))
    | Inst("astore_1","") -> add 1 (JCode.OpStore (`Object, 1))
    | Inst("astore_2","") -> add 1 (JCode.OpStore (`Object, 2))
    | Inst("astore_3","") -> add 1 (JCode.OpStore (`Object, 3))
    | Inst("athrow", "") -> add 1 (JCode.OpThrow)

    (* B *)
    | Inst("baload", "") ->add 1 (JCode.OpArrayLoad `ByteBool)
    | Inst("bastore", "") -> add 1 (JCode.OpArrayStore `ByteBool)
    | InstInt(("bipush", "i"), n) -> add 2 (JCode.OpConst(`Int (Int32.of_int n)))
    | Inst("breakpoint", "") -> add 1 (JCode.OpBreakpoint)

    (* C *)
    | Inst("caload", "") ->add 1 (JCode.OpArrayLoad `Char)
    | Inst("castore", "") -> add 1 (JCode.OpArrayStore `Char)
    | InstWord(("checkcast", "class"), w) ->
      add 3 (JCode.OpCheckCast (JParseSignature.parse_objectType w))

    (* D *)
    | Inst("d2f", "") -> add 1 (JCode.OpD2F)
    | Inst("d2i", "") -> add 1 (JCode.OpD2I)
    | Inst("d2l", "") -> add 1 (JCode.OpD2L)


    | Inst("dadd", "") -> add 1 (JCode.OpAdd `Double)
    | Inst("daload", "") ->add 1 (JCode.OpArrayLoad `Double)
    | Inst("dastore", "") -> add 1 (JCode.OpArrayStore `Double)


    | Inst("dcmpg", "") -> add 1 (JCode.OpCmp `DG)
    | Inst("dcmpl", "") -> add 1 (JCode.OpCmp `DL)

    | Inst("dconst_0", "") -> add 1(JCode.OpConst(`Double (0.)))
    | Inst("dconst_1", "") -> add 1(JCode.OpConst(`Double (1.)))

    | Inst("ddiv", "") -> add 1 (JCode.OpDiv `Double)
    | InstInt(("dload", "i"), 0) -> add 1 (JCode.OpLoad (`Double, 0))
    | InstInt(("dload", "i"), 1) -> add 1 (JCode.OpLoad (`Double, 1))
    | InstInt(("dload", "i"), 2) -> add 1 (JCode.OpLoad (`Double, 2))
    | InstInt(("dload", "i"), 3) -> add 1 (JCode.OpLoad (`Double, 3))
    | InstInt(("dload", "i"), i) -> add 2 (JCode.OpLoad (`Double, i))
    | InstInt(("dload", "I"), 0) -> add 1 (JCode.OpLoad (`Double, 0))
    | InstInt(("dload", "I"), 1) -> add 1 (JCode.OpLoad (`Double, 1))
    | InstInt(("dload", "I"), 2) -> add 1 (JCode.OpLoad (`Double, 2))
    | InstInt(("dload", "I"), 3) -> add 1 (JCode.OpLoad (`Double, 3))
    | InstInt(("dload", "I"), n) -> add 3 (JCode.OpLoad (`Double, n))
    
    | Inst("dload_0", "") -> add 1 (JCode.OpLoad (`Double, 0))
    | Inst("dload_1", "") -> add 1 (JCode.OpLoad (`Double, 1))
    | Inst("dload_2", "") -> add 1 (JCode.OpLoad (`Double, 2))
    | Inst("dload_3", "") -> add 1 (JCode.OpLoad (`Double, 3))

    | Inst("dmul", "") -> add 1 (JCode.OpMult `Double)
    | Inst("dneg", "") -> add 1 (JCode.OpNeg `Double)
    | Inst("drem", "") -> add 1 (JCode.OpRem `Double)
    | Inst("dreturn", "") -> add 1 (JCode.OpReturn `Double)
    | InstInt(("dstore", "i"), 0) -> add 1 (JCode.OpStore (`Double, 0))
    | InstInt(("dstore", "i"), 1) -> add 1 (JCode.OpStore (`Double, 1))
    | InstInt(("dstore", "i"), 2) -> add 1 (JCode.OpStore (`Double, 2))
    | InstInt(("dstore", "i"), 3) -> add 1 (JCode.OpStore (`Double, 3))
    | InstInt(("dstore", "i"), i) -> add 2 (JCode.OpStore (`Double, i))
    | InstInt(("dstore", "I"), 0) -> add 1 (JCode.OpStore (`Double, 0))
    | InstInt(("dstore", "I"), 1) -> add 1 (JCode.OpStore (`Double, 1))
    | InstInt(("dstore", "I"), 2) -> add 1 (JCode.OpStore (`Double, 2))
    | InstInt(("dstore", "I"), 3) -> add 1 (JCode.OpStore (`Double, 3))
    | InstInt(("dstore", "I"), n) -> add 3 (JCode.OpStore (`Double, n))

    | Inst("dstore_0", "") -> add 1 (JCode.OpStore (`Double, 0))
    | Inst("dstore_1", "") -> add 1 (JCode.OpStore (`Double, 1))
    | Inst("dstore_2", "") -> add 1 (JCode.OpStore (`Double, 2))
    | Inst("dstore_3", "") -> add 1 (JCode.OpStore (`Double, 3))
    | Inst("dsub", "") -> add 1 (JCode.OpSub `Double)
    | Inst("dup", "") -> add 1 (JCode.OpDup)
    | Inst("dup2", "") -> add 1 (JCode.OpDup2)
    | Inst("dup2_x1", "") -> add 1 (JCode.OpDup2X1)
    | Inst("dup2_x2", "") -> add 1 (JCode.OpDup2X2)
    | Inst("dup_x1", "") -> add 1 (JCode.OpDupX1)
    | Inst("dup_x2", "") -> add 1 (JCode.OpDupX2)

    (* F *)
    | Inst("f2i", "") -> add 1 (JCode.OpF2I)
    | Inst("f2l", "") -> add 1 (JCode.OpF2L)
    | Inst("f2d", "") -> add 1 (JCode.OpF2D)


    | Inst("fadd", "") -> add 1 (JCode.OpAdd `Float)

    | Inst("faload", "") ->add 1 (JCode.OpArrayLoad `Float)
    | Inst("fastore", "") -> add 1 (JCode.OpArrayStore `Float)

    | Inst("fcmpg", "") -> add 1 (JCode.OpCmp `FG)
    | Inst("fcmpl", "") -> add 1 (JCode.OpCmp `FL)
    | Inst("fconst_0", "") -> add 1(JCode.OpConst(`Float (0.)))
    | Inst("fconst_1", "") -> add 1(JCode.OpConst(`Float (1.)))
    | Inst("fconst_2", "") -> add 1(JCode.OpConst(`Float (2.)))

    | Inst("fdiv", "") -> add 1 (JCode.OpDiv `Float)

    | InstInt(("fload", "i"), 0) -> add 1 (JCode.OpLoad (`Float, 0))
    | InstInt(("fload", "i"), 1) -> add 1 (JCode.OpLoad (`Float, 1))
    | InstInt(("fload", "i"), 2) -> add 1 (JCode.OpLoad (`Float, 2))
    | InstInt(("fload", "i"), 3) -> add 1 (JCode.OpLoad (`Float, 3))
    | InstInt(("fload", "i"), i) -> add 2 (JCode.OpLoad (`Float, i))
    | InstInt(("fload", "I"), 0) -> add 1 (JCode.OpLoad (`Float, 0))
    | InstInt(("fload", "I"), 1) -> add 1 (JCode.OpLoad (`Float, 1))
    | InstInt(("fload", "I"), 2) -> add 1 (JCode.OpLoad (`Float, 2))
    | InstInt(("fload", "I"), 3) -> add 1 (JCode.OpLoad (`Float, 3))
    | InstInt(("fload", "I"), n) -> add 3 (JCode.OpLoad (`Float, n))
    
    | Inst("fload_0", "") -> add 1 (JCode.OpLoad (`Float, 0))
    | Inst("fload_1", "") -> add 1 (JCode.OpLoad (`Float, 1))
    | Inst("fload_2", "") -> add 1 (JCode.OpLoad (`Float, 2))
    | Inst("fload_3", "") -> add 1 (JCode.OpLoad (`Float, 3))

    | Inst("fmul", "") -> add 1 (JCode.OpMult `Float)
    | Inst("fneg", "") -> add 1 (JCode.OpNeg `Float)
    | Inst("frem", "") -> add 1 (JCode.OpRem `Float)
    | Inst("freturn", "") -> add 1 (JCode.OpReturn `Float)
    | InstInt(("fstore", "i"), 0) -> add 1 (JCode.OpStore (`Float, 0))
    | InstInt(("fstore", "i"), 1) -> add 1 (JCode.OpStore (`Float, 1))
    | InstInt(("fstore", "i"), 2) -> add 1 (JCode.OpStore (`Float, 2))
    | InstInt(("fstore", "i"), 3) -> add 1 (JCode.OpStore (`Float, 3))
    | InstInt(("fstore", "i"), i) -> add 2 (JCode.OpStore (`Float, i))
    | InstInt(("fstore", "I"), 0) -> add 1 (JCode.OpStore (`Float, 0))
    | InstInt(("fstore", "I"), 1) -> add 1 (JCode.OpStore (`Float, 1))
    | InstInt(("fstore", "I"), 2) -> add 1 (JCode.OpStore (`Float, 2))
    | InstInt(("fstore", "I"), 3) -> add 1 (JCode.OpStore (`Float, 3))
    | InstInt(("fstore", "I"), n) -> add 3 (JCode.OpStore (`Float, n))

    | Inst("fstore_0", "") -> add 1 (JCode.OpStore (`Float, 0))
    | Inst("fstore_1", "") -> add 1 (JCode.OpStore (`Float, 1))
    | Inst("fstore_2", "") -> add 1 (JCode.OpStore (`Float, 2))
    | Inst("fstore_3", "") -> add 1 (JCode.OpStore (`Float, 3))
    | Inst("fsub", "") -> add 1 (JCode.OpSub `Float)

    (* G *)
    | InstWordWord(("getfield", "field"), cf, fd) ->
      let (c,f) = split_obj(cf) in
      let fd = JParseSignature.parse_field_descriptor(fd) in
      add 3 (JCode.OpGetField ((JBasics.make_cn c), (JBasics.make_fs f fd)))
    | InstWordWord(("getstatic", "field"), cf, fd) ->
      let (c,f) = split_obj(cf) in
      let fd = JParseSignature.parse_field_descriptor(fd) in
      add 3 (JCode.OpGetStatic ((JBasics.make_cn c), (JBasics.make_fs f fd)))

    | InstWord(("goto", "label"), l) ->
      add 3 (JCode.OpGoto(label2int l))
    | InstInt(("goto", "label"), n) ->
      let p = n - !pos in
      add 3 (JCode.OpGoto p)
    | InstInt(("goto_w", "label"), n) ->
      let p = n - !pos in
      add 3 (JCode.OpGoto p)

    (* I *)
    | Inst("i2f", "") -> add 1 (JCode.OpI2F)
    | Inst("i2d", "") -> add 1 (JCode.OpI2D)
    | Inst("i2l", "") -> add 1 (JCode.OpI2L)
    | Inst("iadd", "") -> add 1 (JCode.OpAdd `Int2Bool)

    | Inst("iaload", "") -> add 1 (JCode.OpArrayLoad `Int)
    | Inst("iand", "") -> add 1 (JCode.OpIAnd)
    | Inst("iastore", "") -> add 1 (JCode.OpArrayStore `Int)

    | Inst("iconst_0", "") -> add 1 (JCode.OpConst(`Int (0l)))
    | Inst("iconst_1", "") -> add 1 (JCode.OpConst(`Int (1l)))
    | Inst("iconst_2", "") -> add 1 (JCode.OpConst(`Int (2l)))
    | Inst("iconst_3", "") -> add 1 (JCode.OpConst(`Int (3l)))
    | Inst("iconst_4", "") -> add 1 (JCode.OpConst(`Int (4l)))
    | Inst("iconst_5", "") -> add 1 (JCode.OpConst(`Int (5l)))
    | Inst("iconst_m1", "") -> add 1 (JCode.OpConst(`Int (-1l)))
    | Inst("idiv", "") -> add 1 (JCode.OpDiv `Int2Bool)

    | InstInt(("if_acmpeq", "label"), n) ->
      add 3 (JCode.OpIfCmp (`AEq, (n - !pos)))
    | InstInt(("if_acmpne", "label"), n) ->
      add 3 (JCode.OpIfCmp (`ANe, (n - !pos)))
    | InstInt(("if_icmpeq", "label"), n) ->
      add 3 (JCode.OpIfCmp (`IEq, (n - !pos)))
    | InstInt(("if_icmpge", "label"), n) ->
      add 3 (JCode.OpIfCmp (`IGe, (n - !pos)))
    | InstInt(("if_icmpgt", "label"), n) ->
      add 3 (JCode.OpIfCmp (`IGt, (n - !pos)))
    | InstInt(("if_icmple", "label"), n) ->
      add 3 (JCode.OpIfCmp (`ILe, (n - !pos)))
    | InstInt(("if_icmplt", "label"), n) ->
      add 3 (JCode.OpIfCmp (`ILt, (n - !pos)))
    | InstInt(("if_icmpne", "label"), n) ->
      add 3 (JCode.OpIfCmp (`INe, (n - !pos)))
    | InstInt(("ifeq", "label"), n) ->
      add 3 (JCode.OpIf (`Eq, (n - !pos)))
    | InstInt(("ifge", "label"), n) ->
      add 3 (JCode.OpIf (`Ge, (n - !pos)))
    | InstInt(("ifgt", "label"), n) ->
      add 3 (JCode.OpIf (`Gt, (n - !pos)))
    | InstInt(("ifle", "label"), n) ->
      add 3 (JCode.OpIf (`Le, (n - !pos)))
    | InstInt(("iflt", "label"), n) ->
      add 3 (JCode.OpIf (`Lt, (n - !pos)))
    | InstInt(("ifne", "label"), n) ->
      add 3 (JCode.OpIf (`Ne, (n - !pos)))
    | InstInt(("ifnonnull", "label"), n) ->
      add 3 (JCode.OpIf (`NonNull, (n - !pos)))
    | InstInt(("ifnull", "label"), n) ->
      add 3 (JCode.OpIf (`Null, (n - !pos)))
    | InstIntInt(("iinc", "ii"), i1, i2) -> add 3 (JCode.OpIInc (i1, i2))
    | InstIntInt(("iinc", "Ii"), i1, i2) -> add 3 (JCode.OpIInc (i1, i2))

    | InstInt(("iload", "i"), n) -> add 2 (JCode.OpLoad (`Int2Bool, n))
    | InstInt(("iload", "I"), n) -> add 3 (JCode.OpLoad (`Int2Bool, n))
    | Inst("iload_0", "") -> add 1 (JCode.OpLoad (`Int2Bool, 0))
    | Inst("iload_1", "") -> add 1 (JCode.OpLoad (`Int2Bool, 1))
    | Inst("iload_2", "") -> add 1 (JCode.OpLoad (`Int2Bool, 2))
    | Inst("iload_3", "") -> add 1 (JCode.OpLoad (`Int2Bool, 3))

    | Inst("imul", "") -> add 1 (JCode.OpMult `Int2Bool)
    | Inst("ineg", "") -> add 1 (JCode.OpNeg `Int2Bool)

    | InstWord(("instanceof", "class"), o) ->
      add 3 (JCode.OpInstanceOf (JParseSignature.parse_objectType o))
    | Inst("int2byte", "") -> add 1 (JCode.OpI2B)
    | Inst("int2char", "") -> add 1 (JCode.OpI2C)
    | Inst("int2short", "") -> add 1 (JCode.OpI2S)


    (*
    | Inst("invokedynamic", "method") -> add 1 ()
    | Inst("invokeinterface", "interface") -> add 1 ()
    *)
    | InstWord(("invokenonvirtual", "method"), m) ->
      let (obj,f) = split_method m in
      let (name,o) = split_obj obj in
      let (args,r) = JParseSignature.parse_method_descriptor f in
      add 3(JCode.OpInvoke (`Special (JBasics.make_cn name), (JBasics.make_ms o args r)))
    | InstWord(("invokestatic", "method"), m) ->
      let (obj,f) = split_method m in
      let (name,o) = split_obj obj in
      let (args,r) = JParseSignature.parse_method_descriptor f in
      add 3(JCode.OpInvoke (`Static (JBasics.make_cn name), (JBasics.make_ms o args r)))
    | InstWord(("invokevirtual", "method"), m) ->
      let (obj,f) = split_method m in
      let (name,o) = split_obj obj in
      let (args,r) = JParseSignature.parse_method_descriptor f in
      add 3(JCode.OpInvoke (`Virtual ((JBasics.TClass (JBasics.make_cn name))), (JBasics.make_ms o args r)))

    | Inst("ior", "") -> add 1 (JCode.OpIOr)
    | Inst("irem", "") -> add 1 (JCode.OpRem `Int2Bool)
    | Inst("ireturn", "") -> add 1 (JCode.OpReturn `Int2Bool)
    | Inst("ishl", "") -> add 1 (JCode.OpIShl)
    | Inst("ishr", "") -> add 1 (JCode.OpIShr)
    | InstInt(("istore", "i"), i) -> add 2 (JCode.OpStore (`Int2Bool, i))
    | InstInt(("istore", "I"), i) -> add 3 (JCode.OpStore (`Int2Bool, i))
    | Inst("istore_0", "") -> add 1 (JCode.OpStore (`Int2Bool, 0))
    | Inst("istore_1", "") -> add 1 (JCode.OpStore (`Int2Bool, 1))
    | Inst("istore_2", "") -> add 1 (JCode.OpStore (`Int2Bool, 2))
    | Inst("istore_3", "") -> add 1 (JCode.OpStore (`Int2Bool, 3))
    | Inst("isub", "") -> add 1 (JCode.OpSub `Int2Bool)
    | Inst("iushr", "") -> add 1 (JCode.OpIUShr)
    | Inst("ixor", "") -> add 1 (JCode.OpIXor)

    (* J *)
    | InstWord(("jsr","label"),label) -> Printf.printf "JSR!!! %s\n" label; add 3 (JCode.OpJsr((label2int label)))
    | InstWord(("jsr_w","label"),label) -> add 3 (JCode.OpJsr((label2int label)))

    (* L *)
    | Inst("l2f", "") -> add 1 (JCode.OpL2F)
    | Inst("l2d", "") -> add 1 (JCode.OpL2D)
    | Inst("l2i", "") -> add 1 (JCode.OpL2I)

    | Inst("ladd", "") -> add 1 (JCode.OpAdd `Long)

    | Inst("laload", "") ->add 1 (JCode.OpArrayLoad `Long)
    | Inst("land", "") -> add 1 (JCode.OpLAnd)
    | Inst("lastore", "") -> add 1 (JCode.OpArrayStore `Long)

    | Inst("lcmp", "") -> add 1 (JCode.OpCmp `L)

    | Inst("lconst_0", "") -> add 1 (JCode.OpConst(`Long (Int64.of_int 0)))
    | Inst("lconst_1", "") -> add 1 (JCode.OpConst(`Long (Int64.of_int 1)))

    | InstInt(("ldc", "constant"), n) ->
      add 2 (JCode.OpConst(`Int (Int32.of_int n)));
    | InstStr(("ldc", "constant"), str) ->
      add 2 (JCode.OpConst(`String (JBasics.make_jstr (unstr str))))
    | InstNum(("ldc", "constant"), s) -> add 2 (JCode.OpConst(`Float (float_of_string s)))
    | InstWord(("ldc", "constant"), d) -> add 2 (JCode.OpConst(`Float (float_of_string d)))

    | InstWord(("ldc2_w", "bigconstant"), d) -> add 3 (JCode.OpConst(`Double (float_of_string d)))
    | InstNum(("ldc2_w", "bigconstant"), d) -> add 3 (JCode.OpConst(`Double (float_of_string d)))
    | InstInt(("ldc2_w", "bigconstant"), d) -> add 3 (JCode.OpConst(`Long (Int64.of_int d)))

    | Inst("ldiv", "") -> add 1 (JCode.OpDiv `Long)

    | InstInt(("lload", "i"), i) -> add 2 (JCode.OpLoad (`Long, i)) 
    | InstInt(("lload", "I"), n) -> add 3 (JCode.OpLoad (`Long, n))
    | Inst("lload_0", "") -> add 1 (JCode.OpLoad (`Long, 0))
    | Inst("lload_1", "") -> add 1 (JCode.OpLoad (`Long, 1))
    | Inst("lload_2", "") -> add 1 (JCode.OpLoad (`Long, 2))
    | Inst("lload_3", "") -> add 1 (JCode.OpLoad (`Long, 3))

    | Inst("lmul", "") -> add 1 (JCode.OpMult `Long)
    | Inst("lneg", "") -> add 1 (JCode.OpNeg `Long)
    | LookupSwitch(cases,default) ->
      let a =
        match default with
        | DefaultInt i -> i  - !pos
        | DefaultWord l -> label2int l
      in
      let ls = List.map(function
        | CaseIntInt(i, j)-> (Int32.of_int i, j - !pos)
        | CaseIntWord(i, j) -> (Int32.of_int i, (label2int j))
      ) cases in
      let padding_size = (4 - (!pos + 1) mod 4) mod 4 in
      Format.printf "padding_size=%d ListLen %d\n" padding_size (List.length cases);
      let n = 9 + padding_size + 8 * (List.length cases) in

      add n (JCode.OpLookupSwitch (a, ls))
    | Inst("lor", "") -> add 1 (JCode.OpLOr)
    | Inst("lrem", "") -> add 1 (JCode.OpRem `Long)
    | Inst("lreturn", "") -> add 1 (JCode.OpReturn `Long)
    | Inst("lshl", "") -> add 1 (JCode.OpLShl)
    | Inst("lshr", "") -> add 1 (JCode.OpLShr)

    | InstInt(("lstore", "i"), l) -> add 2 (JCode.OpStore (`Long, l))
    | InstInt(("lstore", "I"), l) -> add 3 (JCode.OpStore (`Long, l))
    | Inst("lstore_0", "") -> add 1  (JCode.OpStore (`Long, 0))
    | Inst("lstore_1", "") -> add 1  (JCode.OpStore (`Long, 1))
    | Inst("lstore_2", "") -> add 1  (JCode.OpStore (`Long, 2))
    | Inst("lstore_3", "") -> add 1  (JCode.OpStore (`Long, 3))
    | Inst("lsub", "") -> add 1 (JCode.OpSub `Long)
    | Inst("lushr", "") -> add 1 (JCode.OpLUShr)
    | Inst("lxor", "") -> add 1 (JCode.OpLXor)

    (* M *)
    | Inst("monitorenter", "") -> add 1 (JCode.OpMonitorEnter)
    | Inst("monitorexit", "") -> add 1 (JCode.OpMonitorExit)
    | InstWordInt(("multianewarray", "marray"), t, i) ->
      add 4 (JCode.OpAMultiNewArray ((JParseSignature.parse_objectType t), i))

    (* N *)
    | InstWord(("new", "class"), o) ->
      let o = replace_dot o in
      add 3 (JCode.OpNew (JBasics.make_cn o))

    | InstWord(("newarray", "atype"), t) ->
      let a = JParseSignature.parse_field_descriptor (cbasic t) in
      add 2 (JCode.OpNewArray a)

    | Inst("nop", "") -> add 1 (JCode.OpNop)

    (* P *)
    | Inst("pop", "") -> add 1 (JCode.OpPop)
    | Inst("pop2", "") -> add 1 (JCode.OpPop2)
    | InstWordWord(("putfield", "field"), cf, fd) ->
      let (c,f) = split_obj(cf) in
      let fd = JParseSignature.parse_field_descriptor(fd) in
      add 3 (JCode.OpPutField ((JBasics.make_cn c), (JBasics.make_fs f fd)))
    | InstWordWord(("putstatic", "field"), cf, fd) ->
      let (c,f) = split_obj(cf) in
      let fd = JParseSignature.parse_field_descriptor(fd) in
      add 3 (JCode.OpPutStatic ((JBasics.make_cn c), (JBasics.make_fs f fd)))

    (* R *)
    | InstInt(("ret", "i"), n) -> add 2 (JCode.OpRet n)
    | InstInt(("ret", "I"), n) -> add 3 (JCode.OpRet n)
    | Inst("return", "") -> add 1 (JCode.OpReturn `Void)

    (* S *)
    | Inst("saload", "") -> add 1 (JCode.OpArrayLoad `Short)
    | Inst("sastore", "") -> add 1 (JCode.OpArrayStore `Short)
    | InstInt(("sipush", "i"), n) -> add 3 (JCode.OpConst(`Short n))
    | Inst("swap", "") -> add 1 (JCode.OpSwap)

    (* T *)
    | TableSwitch((low,high),defs,def) ->
      let default2int = function
        | DefaultInt i -> i - !pos
        | DefaultWord l -> label2int l
      in
      let defs = List.map default2int defs in
      let defs = Array.of_list defs in
      let padding_size = (4 - ((!pos + 1) mod 4)) mod 4 in
      let n = 13 + padding_size + 4 * (Array.length defs) in
      Printf.printf "high = %d\n" high;
      let high = if high = -1 then Array.length defs - 1 else high in
      Printf.printf "high = %d\n" high;
      add n (JCode.OpTableSwitch ((default2int def), Int32.of_int low, Int32.of_int high, defs))

    | Inst(a,b) -> Printf.printf "Inst(%S, %S)\n" a b; add 0 JCode.OpInvalid
    | InstIntInt((a,b),i1,i2) -> Printf.printf "InstIntInt((%S, %S), %d, %d)\n" a b i1 i2; add 0 JCode.OpInvalid
    | InstInt((a,b),i1) -> Printf.printf "InstInt((%S, %S), %d)\n" a b i1; add 0 JCode.OpInvalid
    | InstNum((a,b),s) -> Printf.printf "InstNum((%S, %S), %S)\n" a b s; add 0 JCode.OpInvalid
    | InstWord((a,b),s) -> Printf.printf "InstWord((%S, %S), %S)\n" a b s; add 0 JCode.OpInvalid
    | InstWordInt((a,b),s,i) -> Printf.printf "InstWordInt(%S, %S), %S, %d)\n" a b s i; add 0 JCode.OpInvalid
    | InstWordWord((a,b),s1,s2) -> Printf.printf "InstWordWord((%S, %S), %S, %S)\n" a b s1 s2; add 0 JCode.OpInvalid
    | InstStr((a,b),s) -> Printf.printf "InstStr((%S, %S), %S)\n" a b s; add 0 JCode.OpInvalid
    | InstRelative((a,b),s) -> Printf.printf "InstRelative((%S, %S), %S)\n" a b s; add 0 JCode.OpInvalid
  in
  let codes = List.map f ss in
  let code = Array.create !pos JCode.OpInvalid in
  List.iter (function 
    | (p, JCode.OpInvalid) -> ()
    | (p, JCode.OpJsr(l)) ->
      Printf.printf "opjsr %d\n" l;
      Printf.printf "realloc %d\n" (realloc p l);
      code.(p) <- JCode.OpJsr(realloc p l)
    | (p, JCode.OpGoto(l)) ->
      Printf.printf "opjsr %d\n" l;
      Printf.printf "realloc %d\n" (realloc p l);
      code.(p) <- JCode.OpGoto(realloc p l)

    | (p, JCode.OpLookupSwitch(d,cases)) ->
      code.(p) <- JCode.OpLookupSwitch(realloc p d ,List.map(fun(a,b)-> (a ,realloc p b)) cases)

    | (p, JCode.OpTableSwitch (def, low, high, defs)) ->
      Printf.printf "table p=%d OpTableSwitch(%d, %d, %d, [|%d,%d|])\n" p def (Int32.to_int low) (Int32.to_int high) defs.(0) defs.(1);
      let defs = Array.map (realloc p) defs in
      Printf.printf "table p=%d OpTableSwitch(%d, %d, %d, [|%d,%d|])\n" p (realloc p def) (Int32.to_int low) (Int32.to_int high) defs.(0) defs.(1);
      code.(p) <- JCode.OpTableSwitch(realloc p def,low, high, defs)
    | (p, c) -> code.(p) <- c;      Printf.printf "p=%d\n" p
  ) codes;
  
  code

%}

/* Directives (words beginning with a '.') */
%token DCATCH DCLASS DEND DFIELD DLIMIT DLINE DMETHOD DSET DSUPER
%token DSOURCE DTHROWS DVAR DIMPLEMENTS DINTERFACE DBYTECODE DDEBUG
%token DENCLOSING DSIGNATURE DSTACK DATTRIBUTE DDEPRECATED DINNER
%token DANNOTATION

/* keywords for directives */
%token USING IS FROM METHOD SIGNATURE STACK OFFSET LOCALS FIELD CLASS
%token TO INNER OUTER VISIBLE INVISIBLE VISIBLEPARAM INVISIBLEPARAM USE

/* access types */
%token ABSTRACT FINAL INTERFACE NATIVE PRIVATE PROTECTED PUBLIC STATIC
%token SYNCHRONIZED TRANSIENT VOLATILE
/* added these for java 1.5 compliance : */
%token ANNOTATION ENUM BRIDGE VARARGS STRICT SYNTHETIC

/* complex instructions */
%token LOOKUPSWITCH TABLESWITCH DEFAULT

/* special symbols */
%token EQ SEP COLON

%token <string> Str
%token <string> Word
%token <string * string> Insn
%token <int> Int
%token <string> Num
%token <string> Relative

%token EOF

%start jas_file
%type <Javalib_pack.JCode.jcode Javalib_pack.Javalib.interface_or_class> jas_file

%%

/* The grammar */

jas_file :
  | jasmin_header inners fields methods
    {
      $1 $2 $3 $4
    }

jasmin_header :
  | bytecode_spec
      source_spec
      class_spec
      super_spec
      implements
      signature_spec
      enclosing_spec
      deprecated_spec
      annotations
      generic_attributes
      debug_extension
    {

      let (class_or_interface, access, name) = $3 in

      match class_or_interface with
      | "class"
      | "interface" | _ ->
        (fun inners fields methods ->
        JClass {
          c_name = JBasics.make_cn name;
          c_version = $1;
          c_access = if List.mem `Public access then `Public else `Default;
          c_final = List.mem `Final access;
          c_abstract = List.mem `Abstract access;
          c_super_class = $4;
          c_generic_signature = None;
          c_fields = fields;
          c_interfaces = [];
          c_consts = [||];
          c_sourcefile = $2;
          c_deprecated = $8;
          c_enclosing_method = None;
          c_source_debug_extention = None;
          c_inner_classes = inners;
          c_synthetic = List.mem `Synthetic access;
          c_enum = List.mem `Enum access;
          c_annotations = $9;
          c_other_flags = [];
          c_other_attributes = [];
          c_methods = methods;
        })

    }

/* ---- Signature specification */

signature_spec :
  | DSIGNATURE signature_expr SEP
    {
      ""
    }
  | /* empty */
    {
      ""
    }

signature_expr :
  | Str
    {
      ""
    }

/* ---- Deprecated attribute */

deprecated_spec :
  | DDEPRECATED deprecated_expr SEP
    {
      true
    }
  | /* nothing */
    {
      false
    }

deprecated_expr :
  |
    {
      ""
    }
/* ---- Bytecode version specification */

bytecode_spec :
  | DBYTECODE Num SEP
    {
      let (major, minor) =
        begin try
          let pos = String.index $2 '.' in
          let len = String.length $2 in
          (String.sub $2 0 pos, String.sub $2 (pos+1) (len-pos-1))
        with e -> ($2,"0")
        end
      in
      { JBasics.major = int_of_string major; minor = int_of_string minor }
    }
  | /* nothing */
    {
      { JBasics.major = 45; minor = 3 }
    }


/* ---- Source specification */

source_spec :
  | DSOURCE Str SEP
    {
      Some $2
    }
  | DSOURCE Word SEP
    {
      Some $2
    }
  | /* nothing */
    {
      !sourcefile
    }

/* ---- Class specification */

class_spec :
  | DCLASS access classname SEP
    {
      cn := JBasics.make_cn $3;
      ("class",$2,$3)
    }
  | DINTERFACE access classname SEP
    {
      cn := JBasics.make_cn $3;
      ("interface",$2,$3)
    }

classname :
  | Word
    {
      replace_dot $1
    }

access :
  | access_list
    {
      $1
    }


access_list :
  | access_items
    {
      $1
    }
  |
    {
      [`Default]
    }

access_items :
  | access_item access_items
    {
      $1::$2
    }
  | access_item
    {
      [$1]
    }

access_item :
  | PUBLIC { `Public }
  | PRIVATE { `Private }
  | PROTECTED { `Protected }
  | STATIC { `Static }
  | FINAL { `Final }
  | SYNCHRONIZED { `Synchronized }
  | VOLATILE { `Volatile }
  | TRANSIENT { `Transient }
  | NATIVE { `Native }
  | INTERFACE { `Interface }
  | ABSTRACT { `Abstract }
  | ANNOTATION { `Annotation }
  | ENUM { `Enum }
  | BRIDGE { `Bridge }
  | VARARGS { `Varargs }
  | STRICT { `Strict }
  | SYNTHETIC { `Synthetic }

/* --- Superclass specification */

super_spec :
  | DSUPER classname SEP
    {
      Some (JBasics.make_cn $2)
    }

/* ---- Implements specification */

implements :
  | implements_list
    {
      ""
    }
  | /* empty */
    {
      ""
    }

implements_list :
  | implements_list implements_spec
    {
      ""
    }
  | implements_spec
    {
      ""
    }

implements_spec :
  | DIMPLEMENTS classname SEP
    {
      ""
    }

/* ---- Annotation specification */

annotations :
  | ann_cls_list
    {
      []
    }
  | /* empty */
    {
      []
    }

ann_cls_list :
  | ann_cls_list ann_cls_spec
    {
      ""
    }
  | ann_cls_spec
    {
      ""
    }

ann_cls_spec :
  | ann_cls_expr ann_arglist endannotationsep
    {
      ""
    }

endannotationsep :
  | endannotation SEP
    {
      ""
    }

endannotation :
  | DEND ANNOTATION
    {
      ""
    }

ann_cls_expr :
  | DANNOTATION ann_clf_expr
    {
      ""
    }

ann_clf_expr :
  | VISIBLE classname SEP
    {
      ""
    }
  | INVISIBLE classname SEP
    {
      ""
    }

ann_met_expr :
  | VISIBLE classname SEP
    {
      ""
    }
  | INVISIBLE classname SEP
    {
      ""
    }
  | VISIBLEPARAM Int classname SEP
    {
      ""
    }
  | INVISIBLEPARAM Int classname SEP
    {
      ""
    }

ann_arglist :
  | ann_arg_list
    {
      ""
    }
  | /* empty */
    {
      ""
    }

ann_arg_list :
  | ann_arg_list ann_arg_spec
    {
      ""
    }
  | ann_arg_spec
    {
      ""
    }

ann_arg_spec :
  | ann_arg_expr EQ ann_value_list
    {
      ""
    }

ann_arg_expr :
  | Word Word
    {
      ""
    }
  | Word Word Word
    {
      ""
    }

ann_def_spec :
  | DEFAULT SEP
    {
      ""
    }

ann_value_list :
  | ann_value_items SEP
    {
      ""
    }
  | ann_ann_list
    {
      ""
    }

ann_value_items :
  | ann_value_items ann_value
    {
      ""
    }
  | ann_value
    {
      ""
    }

ann_value :
  | any_item
    {
      ""
    }

ann_ann_list :
  | ann_ann_list ann_ann_value
    {
      ""
    }
  | ann_ann_value
    {
      ""
    }

ann_ann_value :
  | DANNOTATION ann_nest ann_arglist endannotationsep
    {
      ""
    }

ann_nest :
  | SEP
    {
      ""
    }

ann_def_val :
  | ann_def_expr EQ ann_value_list
    {
      ""
    }

ann_def_expr :
  | Word
    {
      ""
    }
  | Word Word
    {
      ""
    }

/* ---- SourceDebugExtension attribute specification */

debug_extension :
  | debug_list
    {
      ""
    }
  | /* empty */
    {
      ""
    }

debug_list :
  | debug_list debug_spec
    {
      ""
    }
  | debug_spec
    {
      ""
    }

debug_spec :
  | DDEBUG Str SEP
    {
      ""
    }


/* ---- EnclosingMethod attribute specification */

enclosing_spec :
  | DENCLOSING METHOD Word SEP
    {
      ""
    }
  | /* nothing */
    {
      ""
    }


/* ---- Generic attributes specification */

generic_attributes :
  | generic_list
    {
      ""
    }
  | /* empty */
    {
      ""
    }

generic_list :
  | generic_list generic_spec
    {
      ""
    }
  | generic_spec
    {
      ""
    }

generic_spec :
  | DATTRIBUTE generic_expr SEP
    {
      ""
    }

generic_expr :
  | Word Str
    {
      ""
    }


/* ---- Fields */

fields :
  | field_list
    {
      List.fold_left (fun fields (fs,f) ->
        JBasics.FieldMap.add fs f fields
      ) JBasics.FieldMap.empty $1
    }
  | { JBasics.FieldMap.empty }

field_list :
  | field_spec field_list { $1::$2 }
  | field_spec { [$1] }

field_spec :
  | DFIELD access Word Word SIGNATURE Str optional_default SEP
    {
      let fs = JBasics.make_fs $3 (JParseSignature.parse_field_descriptor $4) in
      let f = {
        cf_signature = fs;
        cf_class_signature = JBasics.make_cfs !cn fs;
        cf_generic_signature = None;
        cf_access = cf_access $2;
        cf_static = List.mem `Static $2;
        cf_synthetic = List.mem `Synthetic $2;
        cf_enum = List.mem `Enum $2;
        cf_kind = if List.mem `Final $2 then Final else NotFinal;
        cf_value = None;
        cf_transient = false;
        cf_annotations = [];
        cf_other_flags = [];
        cf_attributes = {
          synthetic = false; deprecated = false; other = []
        }
      } in
      (fs,f)
    }
  | DFIELD access Word Word optional_default SEP
    {
      let fs = JBasics.make_fs $3 (JParseSignature.parse_field_descriptor $4) in
      let f = {
        cf_signature = fs;
        cf_class_signature = JBasics.make_cfs !cn fs;
        cf_generic_signature = None;
        cf_access = cf_access $2;
        cf_static = List.mem `Static $2;
        cf_synthetic = List.mem `Synthetic $2;
        cf_enum = List.mem `Enum $2;
        cf_kind = if List.mem `Final $2 then Final else NotFinal;
        cf_value = None;
        cf_transient = false;
        cf_annotations = [];
        cf_other_flags = [];
        cf_attributes = {
          synthetic = false; deprecated = false; other = []
        }
      } in
      (fs,f)
    }
  | DFIELD field_start field_exts endfield
    {
      let fs = JBasics.make_fs $3 (JParseSignature.parse_field_descriptor $4) in
      let f = {
        cf_signature = fs;
        cf_class_signature = JBasics.make_cfs !cn fs;
        cf_generic_signature = None;
        cf_access = `Default;
        cf_static = false;
        cf_synthetic = false;
        cf_enum = false;
        cf_kind = NotFinal;
        cf_value = None;
        cf_transient = false;
        cf_annotations = [];
        cf_other_flags = [];
        cf_attributes = {
          synthetic = false; deprecated = false; other = []
        }
      } in
      (fs,f)
    }

/* default value for a field */
optional_default :
  | EQ item
    {
      ""
    }
  | /* empty */
    {
      ""
    }

/* multiline form of field description */
field_start :
  | access Word Word optional_default SEP
    {
      ""
    }

endfield :
  | DEND FIELD SEP
    {
      ""
    }

field_exts :
  | field_ext_list
    {
      ""
    }
  | /* empty */
    {
      ""
    }

field_ext_list :
  | field_ext_list field_ext_expr
    {
      ""
    }
  | field_ext_expr
    {
      ""
    }

field_ext_expr :
  | DSIGNATURE signature_expr SEP
    {
      ""
    }
  | DATTRIBUTE generic_expr SEP
    {
      ""
    }
  | DDEPRECATED deprecated_expr SEP
    {
      ""
    }
  | DANNOTATION ann_clf_expr ann_arglist endannotationsep
    {
      ""
    }


/* an item is an integer, a float/double/long, or a quoted string  */
item :
  | Int
    {
      ""
    }
  | Num
    {
      ""
    }
  | Str
    {
      ""
    }
/* an item is any possible type */
any_item :
  | Word
    {
      ""
    }
  | item
    {
      ""
    }

/* ---- Inner classes */

inners :
  | inner_list
    {
      []
    }
  | /* empty */
    {
      []
    }

inner_list :
  | inner_list inner_spec
    {
      ""
    }
  | inner_spec
    {
      ""
    }

inner_spec :
  | DINNER CLASS access inner_name inner_inner inner_outer SEP
    {
      ""
    }
  | DINNER INTERFACE access inner_name inner_inner inner_outer SEP
    {
      ""
    }

inner_name :
  | Word
    {
      ""
    }
  | /* empty */
    {
      ""
    }

inner_inner :
  | INNER classname
    {
      ""
    }
  | /* empty */
    {
      ""
    }

inner_outer :
  | OUTER classname
    {
      ""
    }
  | /* empty */
    {
      ""
    }


/* ---- Methods */

methods :
  | method_list {
    List.fold_left (fun methods (ms,m) ->
      JBasics.MethodMap.add ms m methods
    ) JBasics.MethodMap.empty $1
  }
  | /* empty */ { JBasics.MethodMap.empty }

method_list :
  | method_spec { [$1] }
  | method_spec method_list  { $1::$2 }

method_spec :
  | defmethod statements endmethod {
    let(access,ms) = $1 in
    let code = mkcode $2 in
    let jmethod = {
      JCode.c_max_stack = !limit_stack;
      c_max_locals = !limit_locals;
      c_code = code;
      c_exc_tbl = [];
      c_line_number_table = None;
      c_local_variable_table = None;
      c_local_variable_type_table = None;
      c_stack_map_midp = None;
      c_stack_map_java6 = None;
      c_attributes = []
    }
    in  
    let m = ConcreteMethod {
      cm_signature = ms;
      cm_class_method_signature = JBasics.make_cms !cn ms;
      cm_static = List.mem `Static access;
      cm_final = List.mem `Final access;
      cm_synchronized = List.mem `Synchronized access;
      cm_strict = List.mem `Strict access;
      cm_access = cf_access access;
      cm_generic_signature = None;
      cm_bridge = false;
      cm_varargs = false;
      cm_synthetic = false;
      cm_other_flags = [];
      cm_exceptions = [];
      cm_attributes = { synthetic = false; deprecated = false; other = [] };
      cm_annotations = { ma_global = []; ma_parameters = [] };
      cm_implementation = Java (lazy jmethod)
    } in (ms,m)
  }
  | defmethod endmethod {
    let(access,ms) = $1 in
    let code =[||] in
    let jmethod = {
      JCode.c_max_stack = 0;
      c_max_locals = 0;
      c_code = code;
      c_exc_tbl = [];
      c_line_number_table = None;
      c_local_variable_table = None;
      c_local_variable_type_table = None;
      c_stack_map_midp = None;
      c_stack_map_java6 = None;
      c_attributes = []
    }
    in
    let m = ConcreteMethod {
      cm_signature = ms;
      cm_class_method_signature = JBasics.make_cms !cn ms;
      cm_static = List.mem `Static access;
      cm_final = List.mem `Final access;
      cm_synchronized = List.mem `Synchronized access;
      cm_strict = List.mem `Strict access;
      cm_access = cf_access access;
      cm_generic_signature = None;
      cm_bridge = false;
      cm_varargs = false;
      cm_synthetic = false;
      cm_other_flags = [];
      cm_exceptions = [];
      cm_attributes = { synthetic = false; deprecated = false; other = [] };
      cm_annotations = { ma_global = []; ma_parameters = [] };
      cm_implementation = Java (lazy jmethod)
    } in (ms,m)
  }

defmethod :
  | DMETHOD access Word SEP
    {
      limit_stack := 255;
      limit_locals := 255;
      let (name, md) = split_method $3 in
      let (vts, ovt) = JParseSignature.parse_method_descriptor md in

      let ms = JBasics.make_ms name vts ovt
      in ($2, ms)
    }

endmethod :
  | DEND METHOD SEP
    {
      ""
    }

/* ---- Statements in a method */

statements :
  | statement statements { $1::$2 }
  | statement { [$1] }

statement :
  | stmnt SEP { $1 }

stmnt :
  | instruction
    {
      $1
    }
  | directive
    {
      $1
    }
  | error
    {
      Invalid
    }
  | label
    {
      $1
    }
  | /* empty */
    {
      Invalid
    }


/* label: */
label :
  | Word COLON { Label($1) }
  | Int COLON instruction { LabelInst((string_of_int $1), $3) }

/* Directives (.catch, .set, .limit, etc.) */

directive :
  | DVAR var_expr
    {
      Invalid
    }
  | DLIMIT limit_expr
    {
      Invalid
    }
  | DLINE line_expr
    {
      Invalid
    }
  | DTHROWS throws_expr
    {
      Invalid
    }
  | DCATCH catch_expr
    {
      Invalid
    }
  | DSET set_expr
    {
      Invalid
    }
  | DSIGNATURE signature_expr
    {
      Invalid
    }
  | DATTRIBUTE generic_expr
    {
      Invalid
    }
  | DDEPRECATED deprecated_expr
    {
      Invalid
    }
  | DANNOTATION ann_met_expr ann_arglist endannotation
    {
      Invalid
    }
  | DANNOTATION ann_def_spec ann_def_val endannotation
    {
      Invalid
    }
  | DSTACK stackmap
    {
      Invalid
    }

/*        */
/* .stack */
/*        */
stackmap :
  | defstack stack_map_frame_desc endstack
    {
      ""
    }
  | USE defstack_same stack_map_frame_desc endstack
    {
      ""
    }

  defstack_same :
  | defstack_same_expr LOCALS SEP
    {
      ""
    }

  defstack_same_expr :
  | Int
    {
      ""
    }
  | /* empty */
    {
      ""
    }

  defstack :
  | SEP
    {
      ""
    }

  stack_map_frame_desc :
  | stack_offset_def stack_items
    {
      ""
    }

  stack_offset_def :
  | OFFSET Int SEP
    {
      ""
    }
  | OFFSET Word SEP
    {
      ""
    }
  | /* nothing */
    {
      ""
    }

  stack_items :
  | stack_items stack_item
    {
      ""
    }
  | /* nothing */
    {
      ""
    }

  stack_item :
  | stack_item_expr SEP
    {
      ""
    }

  stack_item_expr :
  | LOCALS Word
    {
      ""
    }
  | LOCALS Word Word
    {
      ""
    }
  | LOCALS Word Int
    {
      ""
    }
  | STACK Word
    {
      ""
    }
  | STACK Word Word
    {
      ""
    }
  | STACK Word Int
    {
      ""
    }

  endstack :
  | DEND STACK
    {
      ""
    }

/* */
/* .var <num> is <name> <desc> from StartLab to EndLab */
/* .var <num> is <name> <desc> signature <sign> from StartLab to EndLab */
/* */
var_expr :
  | Int IS Word Word optional_signature FROM Word TO Word
    {
      ""
    }
  | Int IS Word Word optional_signature
    {
      ""
    }
  | Int IS Word Word optional_signature FROM Int TO Int
    {
      ""
    }

/* optional signature specification for a .var */
optional_signature :
  | SIGNATURE Str
    {
      ""
    }
  | /* empty */
    {
      ""
    }


/* .limit stack <val> */
/* .limit locals <val> */

limit_expr :
  | LOCALS Int         /* .limit locals */
    {
      limit_locals := $2
    }
  | STACK Int          /* .limit stack */
    {
      limit_stack := $2
    }
  | Word Int { () }

/* .line <num> */
line_expr :
  | Int { DLine($1) }

/* .throws <class> */
throws_expr :
  | classname
    {
      ""
    }

/* .catch <class> from <label1> to <label2> using <branchlab> */
catch_expr :
  | classname FROM Word TO Word USING Word
    {
      ""
    }
  | classname FROM Int TO Int USING Int
    {
      ""
    }

/* .set <var> = <val> */
set_expr :
  | Word any_item
    {
      ""
    }

instruction :
  | simple_instruction { $1 }
  | complex_instruction { $1 }

/* Various patterns of instruction: */
/*      instruction [<pattern>] */
simple_instruction :
  | Insn { Inst($1) }
  | Insn Int Int { InstIntInt($1,$2,$3) }
  | Insn Int { InstInt($1,$2) }
  | Insn Num { InstNum($1,$2) }
  | Insn Word { InstWord($1,$2) }
  | Insn Word Int { InstWordInt($1,$2,$3) }
  | Insn Word Word { InstWordWord($1,$2,$3) }
  | Insn Str { InstStr($1,$2) }
  | Insn Relative { InstRelative($1,$2) }


/* complex (i.e. multiline) instructions */
/*      lookupswitch <lookup> */
/*      tableswitch  <table> */

complex_instruction :
  | LOOKUPSWITCH lookup
    {
      $2
    }
  | TABLESWITCH table
    {
      $2
    }

/* lookupswitch */
/*     <value> : <label> */
/*     <value> : <label> */
/*     ... */
/*     default : <label> */

lookup :
  | lookup_args lookup_list lookup_default
    {
      LookupSwitch($2,$3)
    }

lookup_args :
  | SEP     /* no arguments to lookupswitch */
    {
      ""
    }


lookup_list :
  | lookup_entry lookup_list
    {
      $1 :: $2
    }
  | lookup_entry
    {
      [$1]
    }

lookup_entry :
  | Int COLON Word SEP
    {
      CaseIntWord($1,$3)
    }
  | Int COLON Int SEP
    {
      CaseIntInt($1,$3)
    }

lookup_default :
  | DEFAULT COLON Word
    {
      DefaultWord $3
    }
  | DEFAULT COLON Int
    {
      DefaultInt $3
    }


/* tableswitch <low> [<high>] */
/*     <label> */
/*     <label> */
/*     ... */
/*     default : <label> */

table :
  | table_args table_list table_default
    {
      TableSwitch($1,$2,$3)
    }

table_args :
  | Int SEP     /* one argument : the <low> parameter */
    {
      ($1,-1)
    }
  | Int Int SEP     /* two arguments : <low> and <high> parameters */
    {
      ($1,$2)
    }


table_list :
  | table_entry table_list
    {
      $1::$2
    }
  | table_entry
    {
      [$1]
    }

table_entry :
  | Word SEP
    {
      DefaultWord($1)
    }
  | Int SEP
    {
      DefaultInt($1)
    }

table_default :
  | DEFAULT COLON Word
    {
      DefaultWord($3)
    }
  | DEFAULT COLON Int
    {
      DefaultInt($3)
    }
