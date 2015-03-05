open Javalib_pack
open JClass
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
