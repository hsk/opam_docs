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

