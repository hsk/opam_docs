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

module PJSignature = struct
  open Javalib_pack
  open JBasics
  open JSignature
  open PJBasics

  let rec pp_typeVariable fmt = function

    | TypeVariable a0 -> Format.fprintf fmt "(@[<hov2>JSignature.TypeVariable@ %S@])" a0

  and show_typeVariable x = Format.asprintf "%a" pp_typeVariable x

  let rec pp_typeArgument fmt = function
    | ArgumentExtends a0 ->
        Format.fprintf fmt "(@[<hov2>JSignature.ArgumentExtends@ %a@])" pp_fieldTypeSignature a0
    | ArgumentInherits a0 ->
        Format.fprintf fmt "(@[<hov2>JSignature.ArgumentInherits@ %a@])" pp_fieldTypeSignature a0
    | ArgumentIs a0 ->
        Format.fprintf fmt "(@[<hov2>JSignature.ArgumentIs@ %a@])" pp_fieldTypeSignature a0
    | ArgumentIsAny -> Format.pp_print_string fmt "JSignature.ArgumentIsAny"

  and show_typeArgument x = Format.asprintf "%a" pp_typeArgument x

  and pp_simpleClassTypeSignature fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JSignature.scts_name = %S;@ " x.scts_name;
      Format.fprintf fmt "scts_type_arguments = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_typeArgument fmt x;
            true
          ) false x.scts_type_arguments
        end;
        Format.fprintf fmt "@]]"
      end
    end;
    Format.fprintf fmt "@] }"

  and show_simpleClassTypeSignature x = Format.asprintf "%a" pp_simpleClassTypeSignature x

  and pp_classTypeSignature fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JSignature.cts_package = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "%S" x;
            true
          ) false x.cts_package
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "cts_enclosing_classes = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_simpleClassTypeSignature fmt x;
            true
          ) false x.cts_enclosing_classes
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "cts_simple_class_type_signature = %a"
        pp_simpleClassTypeSignature x.cts_simple_class_type_signature
    end;
    Format.fprintf fmt "@] }"

  and show_classTypeSignature x = Format.asprintf "%a" pp_classTypeSignature x

  and pp_formalTypeParameter fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JSignature.ftp_name = %S;@ " x.ftp_name;
      Format.fprintf fmt "ftp_class_bound = ";
      begin
        match x.ftp_class_bound with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " pp_fieldTypeSignature x
      end;
      Format.fprintf fmt "ftp_interface_bounds = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_fieldTypeSignature fmt x;
            true
          ) false x.ftp_interface_bounds
        end;
        Format.fprintf fmt "@]]"
      end
    end;
    Format.fprintf fmt "@] }"

  and show_formalTypeParameter x = Format.asprintf "%a" pp_formalTypeParameter x

  and pp_throwsSignature fmt = function
    | ThrowsClass a -> Format.fprintf fmt "(@[<hov2>JSignature.ThrowsClass@ %a@])" pp_classTypeSignature a
    | ThrowsTypeVariable a -> Format.fprintf fmt "(@[<hov2>JSignature.ThrowsTypeVariable@ %a@])" pp_typeVariable a

  and show_throwsSignature x = Format.asprintf "%a" pp_throwsSignature x

  and pp_typeSignature fmt = function
    | GBasic a -> Format.fprintf fmt "(@[<hov2>JSignature.GBasic@ %a@])" pp_java_basic_type a
    | GObject a -> Format.fprintf fmt "(@[<hov2>JSignature.GObject@ %a@])" pp_fieldTypeSignature a

  and show_typeSignature x = Format.asprintf "%a" pp_typeSignature x

  and pp_classSignature fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JSignature.cs_formal_type_parameters = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_formalTypeParameter fmt x;
            true
          ) false x.cs_formal_type_parameters
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "cs_super_class = %a;@ " pp_classTypeSignature x.cs_super_class;
      Format.fprintf fmt "cs_super_interfaces = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_classTypeSignature fmt x;
            true
          ) false x.cs_super_interfaces
        end;
        Format.fprintf fmt "@]]"
      end
    end;
    Format.fprintf fmt "@] }"

  and show_classSignature x = Format.asprintf "%a" pp_classSignature x

  and pp_fieldTypeSignature fmt = function
    | GClass a -> Format.fprintf fmt "(@[<hov2>JSignature.GClass@ %a@])" pp_classTypeSignature a
    | GArray a -> Format.fprintf fmt "(@[<hov2>JSignature.GArray@ %a@])" pp_typeSignature a
    | GVariable a -> Format.fprintf fmt "(@[<hov2>JSignature.GVariable@ %a@])" pp_typeVariable a

  and show_fieldTypeSignature x = Format.asprintf "%a" pp_fieldTypeSignature x

  let rec pp_methodTypeSignature fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "JSignature.mts_formal_type_parameters = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_formalTypeParameter fmt x;
            true
          ) false x.mts_formal_type_parameters
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "mts_type_signature = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_typeSignature fmt x;
            true
          ) false x.mts_type_signature
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "mts_return_type = ";
      begin
        match x.mts_return_type with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " pp_typeSignature x
      end;
      Format.fprintf fmt "mts_throws = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_throwsSignature fmt x;
            true
          ) false x.mts_throws
        end;
        Format.fprintf fmt "@]]"
      end
    end;
    Format.fprintf fmt "@] }"

  and show_methodTypeSignature x = Format.asprintf "%a" pp_methodTypeSignature x

end

module PJCode = struct
  open Javalib_pack
  open JBasics
  open JCode

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
    | OpGoto a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpGoto@ %d@])" a0
    | OpJsr a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpJsr@ %d@])" a0
    | OpRet a0 -> Format.fprintf fmt "(@[<hov2>JCode.OpRet@ %d@])" a0
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
end

module PJClass = struct

  open Javalib_pack
  open Javalib
  open PJBasics
  open PJCode

  let jclass = ""

  let rec pp_access fmt = function
    | `Default -> Format.pp_print_string fmt "`Default"
    | `Public -> Format.pp_print_string fmt "`Public"
    | `Private -> Format.pp_print_string fmt "`Private"
    | `Protected -> Format.pp_print_string fmt "`Protected"

  and show_access x = Format.asprintf "%a" pp_access x

  let rec pp_attributes fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%ssynthetic = %B;@ " jclass x.synthetic;
      Format.fprintf fmt "deprecated = %B;@ " x.deprecated;
      Format.fprintf fmt "other = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%S,@ %S@])" a0 a1;
            true
          ) false x.other
        end;
        Format.fprintf fmt "@]]"
      end;
    end;
    Format.fprintf fmt "@] }"

  and show_attributes x = Format.asprintf "%a" pp_attributes x

  let rec pp_visibility fmt = function
    | RTVisible -> Format.fprintf fmt "%sRTVisible" jclass
    | RTInvisible -> Format.fprintf fmt "%sRTInvisible" jclass

  and show_visibility x = Format.asprintf "%a" pp_visibility x

  let rec pp_field_kind fmt = function
    | NotFinal -> Format.fprintf fmt "%sNotFinal" jclass
    | Final -> Format.fprintf fmt "%sFinal" jclass
    | Volatile -> Format.fprintf fmt "%sVolatile" jclass

  and show_field_kind x = Format.asprintf "%a" pp_field_kind x

  let rec pp_class_field fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%scf_signature = %a;@ " jclass PJBasics.pp_field_signature x.cf_signature;
      Format.fprintf fmt "cf_class_signature = %a;@ " PJBasics.pp_class_field_signature x.cf_class_signature;
      Format.fprintf fmt "cf_generic_signature = ";
      begin
        match x.cf_generic_signature with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " PJSignature.pp_fieldTypeSignature x
      end;
      Format.fprintf fmt "cf_access = %a;@ " pp_access x.cf_access;
      Format.fprintf fmt "cf_static = %B;@ " x.cf_static;
      Format.fprintf fmt "cf_synthetic = %B;@ " x.cf_synthetic;
      Format.fprintf fmt "cf_enum = %B;@ " x.cf_enum;
      Format.fprintf fmt "cf_kind = %a;@ " pp_field_kind x.cf_kind;
      Format.fprintf fmt "cf_value = ";
      begin
        begin match x.cf_value with
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" pp_constant_value x
        end;
        Format.fprintf fmt ";@ "
      end;
      Format.fprintf fmt "cf_transient = %B;@ " x.cf_transient;
      Format.fprintf fmt "cf_annotations = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>";
            PJBasics.pp_annotation fmt a0;
            Format.fprintf fmt ",@ ";
            pp_visibility fmt a1;
            Format.fprintf fmt "@])";
            true
          ) false x.cf_annotations
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "cf_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "%d" x;
            true
          ) false x.cf_other_flags
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "cf_attributes = %a" pp_attributes x.cf_attributes;
    end;
    Format.fprintf fmt "@] }"

  and show_class_field x = Format.asprintf "%a" pp_class_field x

  let rec pp_interface_field fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%sif_signature = %a;@ " jclass PJBasics.pp_field_signature x.if_signature;
      Format.fprintf fmt "if_class_signature = %a;@ " PJBasics.pp_class_field_signature x.if_class_signature;
      Format.pp_print_string fmt "if_generic_signature = ";
      begin
        match x.if_generic_signature with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " PJSignature.pp_fieldTypeSignature x
      end;
      Format.fprintf fmt "if_synthetic = %B;@ " x.if_synthetic;
      Format.pp_print_string fmt "if_value = ";
      begin
        match x.if_value with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " pp_constant_value x
      end;
      Format.fprintf fmt "if_annotations = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_annotation a0 pp_visibility a1;
            true
          ) false x.if_annotations
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "if_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "%d" x;
            true
          ) false x.if_other_flags
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "if_attributes = %a" pp_attributes x.if_attributes;
    end;
    Format.fprintf fmt "@] }"

  and show_interface_field x = Format.asprintf "%a" pp_interface_field x

  let rec pp_any_field fmt = function
    | InterfaceField a0 ->
        Format.fprintf fmt "(@[<hov2>%sInterfaceField@ %a@])" jclass pp_interface_field a0
    | ClassField a0 ->
        Format.fprintf fmt "(@[<hov2>%sClassField@ %a@])" jclass pp_class_field a0;

  and show_any_field x = Format.asprintf "%a" pp_any_field x

  let rec pp_implementation poly_a fmt = function
  | Native -> Format.fprintf fmt "%sNative" jclass
  | Java(l) ->
    Format.fprintf fmt "(@[<hov2>%sJava (lazy(@\n" jclass;
    poly_a fmt (Lazy.force l);
    Format.fprintf fmt "))@])"

  let rec pp_method_annotations fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%sma_global = [@[<hov>" jclass;
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_annotation a0 pp_visibility a1;
            true
          ) false x.ma_global
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "ma_parameters = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "[@[<hov>";
            ignore begin List.fold_left
              (fun sep (a0,a1) ->
                if sep then Format.fprintf fmt ";@ ";
                Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_annotation a0 pp_visibility a1;
                true
              ) false x
            end;
            Format.fprintf fmt "@]]";
            true
          ) false x.ma_parameters
        end;
        Format.fprintf fmt "@]]"
      end
    end;
    Format.fprintf fmt "@] }"

  and show_method_annotations x = Format.asprintf "%a" pp_method_annotations x

  let rec pp_concrete_method poly_a fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%scm_signature =@\n  @[%a;@]@\n" jclass PJBasics.pp_method_signature x.cm_signature;
      Format.fprintf fmt "cm_class_method_signature =@\n  @[%a;@]@\n" PJBasics.pp_class_method_signature x.cm_class_method_signature;
      Format.fprintf fmt "cm_static = %B;@\n" x.cm_static;
      Format.fprintf fmt "cm_final = %B;@\n" x.cm_final;
      Format.fprintf fmt "cm_synchronized = %B;@\n" x.cm_synchronized;
      Format.fprintf fmt "cm_strict = %B;@\n" x.cm_strict;
      Format.fprintf fmt "cm_access = %a;@\n" pp_access x.cm_access;
      Format.fprintf fmt "cm_generic_signature = ";
      begin
        match x.cm_generic_signature with
        | None -> Format.fprintf fmt "None;@\n"
        | Some x -> Format.fprintf fmt "(Some %a);@\n" PJSignature.pp_methodTypeSignature x
      end;
      Format.fprintf fmt "cm_bridge = %B;@\n" x.cm_bridge;
      Format.fprintf fmt "cm_varargs = %B;@\n" x.cm_varargs;
      Format.fprintf fmt "cm_synthetic = %B;@\n" x.cm_synthetic;
      Format.fprintf fmt "cm_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            Format.fprintf fmt "%d" x;
            true
          ) false x.cm_other_flags
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "cm_exceptions = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            PJBasics.pp_class_name fmt x;
            true
          ) false x.cm_exceptions
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "cm_attributes = %a;@\n" pp_attributes x.cm_attributes;
      Format.fprintf fmt "cm_annotations = %a;@\n" pp_method_annotations x.cm_annotations;
      Format.fprintf fmt "cm_implementation =@\n  @[%a@]" (pp_implementation poly_a)x.cm_implementation;
    end;
    Format.fprintf fmt "@]@\n}"

  and show_concrete_method poly_a x = Format.asprintf "%a" (pp_concrete_method poly_a) x

  let rec pp_abstract_method fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%sam_signature = %a;@\n" jclass PJBasics.pp_method_signature x.am_signature;
      Format.fprintf fmt "am_class_method_signature = %a;@\n" PJBasics.pp_class_method_signature x.am_class_method_signature;
      Format.fprintf fmt "am_access = ";
      begin
        match x.am_access with
        | `Public -> Format.fprintf fmt "`Public;@\n"
        | `Protected -> Format.fprintf fmt "`Protected;@\n"
        | `Default -> Format.fprintf fmt "`Default;@\n"
      end;
      Format.fprintf fmt "am_generic_signature = ";
      begin
        match x.am_generic_signature with
        | None -> Format.fprintf fmt "None;@\n"
        | Some x -> Format.fprintf fmt "(Some %a);@\n" PJSignature.pp_methodTypeSignature x
      end;
      Format.fprintf fmt "am_bridge = %B;@\n" x.am_bridge;
      Format.fprintf fmt "am_varargs = %B;@\n" x.am_varargs;
      Format.fprintf fmt "am_synthetic = %B;@\n" x.am_synthetic;
      Format.fprintf fmt "am_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            Format.fprintf fmt "%d" x;
            true
          ) false x.am_other_flags
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "am_exceptions = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            PJBasics.pp_class_name fmt x;
            true
          ) false x.am_exceptions
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "am_attributes = %a;@\n" pp_attributes x.am_attributes;
      Format.fprintf fmt "am_annotations = %a;@\n" pp_method_annotations x.am_annotations;
      Format.fprintf fmt "am_annotation_default = ";
      begin
        match x.am_annotation_default with
        | None -> Format.pp_print_string fmt "None"
        | Some x -> Format.fprintf fmt "(Some %a)" pp_element_value x
      end
    end;
    Format.fprintf fmt "@] }"

  and show_abstract_method x = Format.asprintf "%a" pp_abstract_method x

  let rec pp_jmethod poly_a fmt = function
    | AbstractMethod a0 ->
        Format.fprintf fmt "(@[<hov2>%sAbstractMethod@ %a@])" jclass pp_abstract_method a0
    | ConcreteMethod a0 ->
        Format.fprintf fmt "(@[<hov2>%sConcreteMethod@ %a@])" jclass
          (pp_concrete_method poly_a) a0

  and show_jmethod poly_a x = Format.asprintf "%a" (pp_jmethod poly_a) x

  let rec pp_inner_class fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%sic_class_name = " jclass;
      begin
        match x.ic_class_name with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " PJBasics.pp_class_name x
      end;
      Format.fprintf fmt "ic_outer_class_name = ";
      begin
        match x.ic_outer_class_name with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " PJBasics.pp_class_name x
      end;
      Format.fprintf fmt "ic_source_name = ";
      begin
        match x.ic_source_name with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %S);@ " x
      end;
      Format.fprintf fmt "ic_access = %a;@ " pp_access x.ic_access;
      Format.fprintf fmt "ic_static = %B;@ " x.ic_static;
      Format.fprintf fmt "ic_final = %B;@ " x.ic_final;
      Format.fprintf fmt "ic_synthetic = %B;@ " x.ic_synthetic;
      Format.fprintf fmt "ic_annotation = %B;@ " x.ic_annotation;
      Format.fprintf fmt "ic_enum = %B;@ " x.ic_enum;
      Format.fprintf fmt "ic_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "%d" x;
            true
          ) false x.ic_other_flags
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "ic_type = %s"
        begin
          match x.ic_type with
          | `ConcreteClass -> "`ConcreteClass"
          | `Abstract -> "`Abstract"
          | `Interface -> "`Interface"
        end;
    end;
    Format.fprintf fmt "@] }"

  and show_inner_class x = Format.asprintf "%a" pp_inner_class x

  let rec pp_jclass poly_a fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%sc_name = %a;@\n" jclass PJBasics.pp_class_name x.c_name;
      Format.fprintf fmt "c_version = %a;@\n" pp_version x.c_version;
      Format.fprintf fmt "c_access = %s;@\n"
        begin match x.c_access with
          | `Public -> "`Public"
          | `Default -> "`Default"
        end;
      Format.fprintf fmt "c_final = %B;@\n" x.c_final;
      Format.fprintf fmt "c_abstract = %B;@\n" x.c_abstract;
      Format.pp_print_string fmt "c_super_class = ";
      begin
        match x.c_super_class with
        | None -> Format.fprintf fmt "None;@\n"
        | Some x -> Format.fprintf fmt "(Some %a);@\n" PJBasics.pp_class_name x
      end;
      Format.pp_print_string fmt "c_generic_signature = ";
      begin
        match x.c_generic_signature with
        | None -> Format.fprintf fmt "None;@\n"
        | Some x -> Format.fprintf fmt "(Some %a);@\n" PJSignature.pp_classSignature x
      end;

      Format.fprintf fmt "c_fields = @[<hov>begin@ ";
      begin
        Format.fprintf fmt "let@ map@ =@ JBasics.FieldMap.empty@ in@ ";
        JBasics.FieldMap.iter (fun k v ->
          Format.fprintf fmt "let@ map@ =@ JBasics.FieldMap.add@ %a@ %a@ map in@ "
            pp_field_signature k pp_class_field v
        ) x.c_fields;
        Format.fprintf fmt "map@]@ end;@\n";
      end;

      Format.fprintf fmt "c_interfaces = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            PJBasics.pp_class_name fmt x;
            true
          ) false x.c_interfaces
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "c_consts = [|@\n  @[<hov>";
      begin
        ignore begin Array.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            pp_constant fmt x;
            true
          ) false x.c_consts
        end;
        Format.fprintf fmt "@]@\n|];@\n"
      end;
      Format.pp_print_string fmt "c_sourcefile = ";
      begin
        match x.c_sourcefile with
        | None -> Format.fprintf fmt "None;@\n"
        | Some x -> Format.fprintf fmt "(Some %S);@\n" x
      end;
      Format.fprintf fmt "c_deprecated = %B;@\n" x.c_deprecated;
      Format.pp_print_string fmt "c_enclosing_method = ";
      begin
        begin match x.c_enclosing_method with
        | None -> Format.pp_print_string fmt "None"
        | Some (a0,a1) ->
            Format.pp_print_string fmt "(Some ";
            Format.fprintf fmt "(@[<hov>";
            PJBasics.pp_class_name fmt a0;
            Format.fprintf fmt ",@ ";
            begin match a1 with
            | None -> Format.pp_print_string fmt "None"
            | Some x -> Format.fprintf fmt "(Some %a)" PJBasics.pp_method_signature x
            end;
            Format.fprintf fmt "@]))"
        end;
        Format.fprintf fmt ";@\n"
      end;
      Format.pp_print_string fmt "c_source_debug_extention = ";
      begin
        match x.c_source_debug_extention with
        | None -> Format.fprintf fmt "None;@\n"
        | Some x -> Format.fprintf fmt "(Some %S);@\n" x;
      end;
      Format.fprintf fmt "c_inner_classes = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            pp_inner_class fmt x;
            true
          ) false x.c_inner_classes
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "c_synthetic = %B;@\n" x.c_synthetic;
      Format.fprintf fmt "c_enum = %B;@\n" x.c_enum;
      Format.fprintf fmt "c_annotations = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@\n";
            Format.fprintf fmt "(@[<hov>";
            pp_annotation fmt a0;
            Format.fprintf fmt ",@ ";
            pp_visibility fmt a1;
            Format.fprintf fmt "@])";
            true
          ) false x.c_annotations
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "c_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@\n";
            Format.fprintf fmt "%d" x;
            true
          ) false x.c_other_flags
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "c_other_attributes = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@\n";
            Format.fprintf fmt "(@[<hov>%S,@ %S@])" a0 a1;
            true
          ) false x.c_other_attributes
        end;
        Format.fprintf fmt "@]];@\n"
      end;
      Format.fprintf fmt "c_methods = begin@\n  @[<hov>";
      begin
        Format.fprintf fmt "let@ methods@ =@ JBasics.MethodMap.empty@ in@\n";
        JBasics.MethodMap.iter (fun k v ->
          Format.fprintf fmt "let@ methods@ =@ JBasics.MethodMap.add@ %a@ %a@ methods in@\n"
            pp_method_signature k (pp_jmethod poly_a) v;
        ) x.c_methods;
        Format.fprintf fmt "methods@]@\nend;@\n";
      end;

    end;
    Format.fprintf fmt "@] }"

  and show_jclass poly_a x = Format.asprintf "%a" (pp_jclass poly_a) x

  let rec pp_jinterface poly_a fmt x =
    Format.fprintf fmt "{ @[<hov>";
    begin
      Format.fprintf fmt "%si_name = %a;@ " jclass PJBasics.pp_class_name x.i_name;
      Format.fprintf fmt "i_version = %a;@ " pp_version x.i_version;
      Format.fprintf fmt "i_access = %s;@ "
        begin match x.i_access with
          | `Public -> "`Public"
          | `Default -> "`Default"
        end;
      Format.fprintf fmt "i_interfaces = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            PJBasics.pp_class_name fmt x;
            true
          ) false x.i_interfaces
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.pp_print_string fmt "i_generic_signature = ";
      begin
        match x.i_generic_signature with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %a);@ " PJSignature.pp_classSignature x
      end;
      Format.fprintf fmt "i_consts = [|@[<hov>";
      begin
        ignore begin Array.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_constant fmt x;
            true
          ) false x.i_consts
        end;
        Format.fprintf fmt "@]|];@ "
      end;
      Format.pp_print_string fmt "i_sourcefile = ";
      begin
        match x.i_sourcefile with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %S);@ " x
      end;
      Format.fprintf fmt "i_deprecated = %B;@ " x.i_deprecated;
      Format.pp_print_string fmt "i_source_debug_extention = ";
      begin
        match x.i_source_debug_extention with
        | None -> Format.fprintf fmt "None;@ "
        | Some x -> Format.fprintf fmt "(Some %S);@ " x
      end;
      Format.fprintf fmt "i_inner_classes = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            pp_inner_class fmt x;
            true
          ) false x.i_inner_classes
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.pp_print_string fmt "i_initializer = ";
      begin
        match x.i_initializer with
        | None -> Format.fprintf fmt "None;@ "
        | Some x ->
            Format.fprintf fmt "(Some %a);@ " (pp_concrete_method poly_a) x;
      end;
      Format.fprintf fmt "i_annotation = %B;@ " x.i_annotation;
      Format.fprintf fmt "i_annotations = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%a,@ %a@])" pp_annotation a0 pp_visibility a1;
            true
          ) false x.i_annotations
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "i_other_attributes = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep (a0,a1) ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "(@[<hov>%S,@ %S@])" a0 a1;
            true
          ) false x.i_other_attributes
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "i_other_flags = [@[<hov>";
      begin
        ignore begin List.fold_left
          (fun sep x ->
            if sep then Format.fprintf fmt ";@ ";
            Format.fprintf fmt "%d" x;
            true
          ) false x.i_other_flags
        end;
        Format.fprintf fmt "@]];@ "
      end;
      Format.fprintf fmt "i_fields = @[<hov>begin@ ";
      begin
        Format.fprintf fmt "let@ map@ =@ JBasics.FieldMap.empty@ in@ ";
        JBasics.FieldMap.iter (fun k v ->
          Format.fprintf fmt "let@ map@ =@ JBasics.FieldMap.add@ %a@ %a@ map in@ "
            pp_field_signature k pp_interface_field v
        ) x.i_fields;
        Format.fprintf fmt "map@]@ end;@ ";
      end;
      Format.fprintf fmt "i_methods = @[<hov>begin@ ";
      begin
        Format.fprintf fmt "let@ map@ =@ JBasics.MethodMap.empty@ in@ ";
        JBasics.MethodMap.iter (fun k v ->
          Format.fprintf fmt "let@ map@ =@ JBasics.MethodMap.add@ %a@ %a@ map in@ "
            pp_method_signature k pp_abstract_method v;
        ) x.i_methods;
        Format.fprintf fmt "map@]@ end;@ ";
      end;
    end;
    Format.fprintf fmt "@] }"

  and show_jinterface poly_a x = Format.asprintf "%a" (pp_jinterface poly_a) x

  let rec pp_interface_or_class poly_a fmt = function
    | JInterface a0 ->
        Format.fprintf fmt "(@[<hov2>%sJInterface@ %a@])" jclass
          (pp_jinterface poly_a) a0
    | JClass a0 ->
        Format.fprintf fmt "(@[<hov2>%sJClass@ %a@])" jclass
          (pp_jclass poly_a) a0

  and show_interface_or_class poly_a x = Format.asprintf "%a" (pp_interface_or_class poly_a) x

  let pp = pp_interface_or_class
  let show = show_interface_or_class
end

open Javalib_pack

let _ =
  let jasmin = ref false in
  let files = ref [] in
  Arg.parse
    [("-jasmin", Arg.Unit(fun () -> jasmin := true), "output jasmin")]
    (fun s -> files := !files @ [s])
    ("javalib decompiler\n" ^
     Printf.sprintf "usage: %s [-jasmin] classfilename" Sys.argv.(0));
  List.iter begin fun name ->
    let aname = JBasics.make_cn name in
    let class_path = Javalib.class_path "./" in
    let a = Javalib.get_class class_path aname in
    if !jasmin then
      Javalib.JPrint.print_jasmin a stdout
    else begin
      Format.printf "open Javalib_pack\n";
      Format.printf "open Javalib\n";
      Format.printf "let _ =\nlet k =@.";
      Format.printf "%a@." (PJClass.pp PJCode.pp_jcode) a;
      Format.printf "  in Javalib.unparse_class k (open_out %S);\n" (name ^ ".class");
      Format.printf "  JPrint.print_jasmin k stdout;\n\n"
    end
  end !files

