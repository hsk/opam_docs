open Javalib_pack
open Javalib
open JClass
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

let rec pp_jstr fmt str = Format.fprintf fmt "(JBasics.make_jstr %S)" (jstr_raw str)

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
