
module PJBasics = struct
type class_name = int * string
[@@deriving show]

(* Numerical types that are not smaller than int. *)
type other_num = [
| `Long
| `Float
| `Double
]
[@@deriving show]

(* JVM basic type (int = short = char = byte = bool). *)
type jvm_basic_type = [
| `Int2Bool
| other_num
]
[@@deriving show]

(* JVM type (int = short = char = byte = bool, all objects have the same type). *)
type jvm_type = [
| jvm_basic_type
| `Object
]
[@@deriving show]

(* JVM array element type (byte = bool, all objects have the same type). *)
type jvm_array_type = [
| `Int
| `Short
| `Char
| `ByteBool
| other_num
| `Object
]
[@@deriving show]

(* JVM return type (byte = bool, all objects have the same type). *)
type jvm_return_type = [
|  jvm_basic_type
| `Object
| `Void
]
[@@deriving show]

(* Java basic type. *)
type java_basic_type = [
| `Int
| `Short
| `Char
| `Byte
| `Bool
| other_num
]
[@@deriving show]

(* Java object type *)
type object_type =
  | TClass of class_name
  | TArray of value_type
[@@deriving show]

(* Java type *)
and value_type =
  | TBasic of java_basic_type
  | TObject of object_type
[@@deriving show]

(* Field descriptor *)
type field_descriptor = value_type
[@@deriving show]

(* Method descriptor *)
type method_descriptor = value_type list * value_type option
[@@deriving show]

type field_signature_data = string * field_descriptor
[@@deriving show]
type method_signature_data = string * method_descriptor
[@@deriving show]

type method_signature = int * method_signature_data
[@@deriving show]
type field_signature = int * field_signature_data
[@@deriving show]

type class_field_signature_data = class_name * field_signature
[@@deriving show]
type class_method_signature_data = class_name * method_signature
[@@deriving show]
type class_field_signature = int * class_field_signature_data
[@@deriving show]
type class_method_signature = int * class_method_signature_data
[@@deriving show]

(* Signatures parsed from CONSTANT_NameAndType_info structures. *)
type descriptor =
  | SValue of field_descriptor
  | SMethod of method_descriptor
[@@deriving show]

(* Constant value. *)
type jstr = string
[@@deriving show]


type constant_value =
  | ConstString of jstr
  | ConstInt of int32
  | ConstFloat of float
  | ConstLong of int64
  | ConstDouble of float
  | ConstClass of object_type
[@@deriving show]

(* Constant. *)
type constant =
  | ConstValue of constant_value
  | ConstField of (class_name * field_signature)
  | ConstMethod of (object_type * method_signature)
  | ConstInterfaceMethod of (class_name * method_signature)
  | ConstNameAndType of string * descriptor
  | ConstStringUTF8 of string
  | ConstUnusable
[@@deriving show]

(* Stackmap type. *)
type verification_type =
  | VTop
  | VInteger
  | VFloat
  | VDouble
  | VLong
  | VNull
  | VUninitializedThis
  | VObject of object_type
  | VUninitialized of int (* creation point *)
[@@deriving show]

type stackmap = (int * verification_type list * verification_type list)
[@@deriving show]

type version = {major :int; minor:int;}
[@@deriving show]

exception No_class_found of string
[@@deriving show]

exception Class_structure_error of string
[@@deriving show]

(* Annotations *)

type element_value =
  | EVCstByte of int
  | EVCstChar of int
  | EVCstInt of int32
  | EVCstShort of int
  | EVCstBoolean of int
  | EVCstDouble of float
  | EVCstFloat of float
  | EVCstLong of int64
  | EVCstString of string
  | EVEnum of (class_name * string)
      (* (type_name_index,const_name_index) cf. JLS 13.1 *)
  | EVClass of value_type option
  | EVAnnotation of annotation
  | EVArray of element_value list

and annotation = {
  kind : class_name;
  element_value_pairs : (string * element_value) list;
}
[@@deriving show]

end

module PPJSignature = struct

open PJBasics


(** {2 Types used in type declarations of generic signatures} *)

(** This is the type used for type variables as P in Collection<P>.*)
type typeVariable = TypeVariable of string
[@@deriving show]

type typeArgument =
  | ArgumentExtends of fieldTypeSignature (** e.g. <?+Object> *)
  | ArgumentInherits of fieldTypeSignature (** e.g. <?-Object> *)
  | ArgumentIs of fieldTypeSignature (** e.g. <Object>*)
  | ArgumentIsAny (** <*> *)

and simpleClassTypeSignature = {
  scts_name : string;
  scts_type_arguments : typeArgument list;
}
and classTypeSignature = {
  cts_package : string list;
  cts_enclosing_classes : simpleClassTypeSignature list;
  cts_simple_class_type_signature : simpleClassTypeSignature;
}
and formalTypeParameter = {
  ftp_name : string;
  ftp_class_bound : fieldTypeSignature option;
  ftp_interface_bounds : fieldTypeSignature list;
}

and throwsSignature =
  | ThrowsClass of classTypeSignature
  | ThrowsTypeVariable of typeVariable

(** [typeSignature] is used for method parameters and return values of
    generic methods. *)
and typeSignature =
  | GBasic of java_basic_type
  | GObject of fieldTypeSignature


(** {2 Types of generic signatures} *)

and classSignature = {
  cs_formal_type_parameters : formalTypeParameter list;
  cs_super_class : classTypeSignature;
  cs_super_interfaces : classTypeSignature list;
}

(** This type is for references. Generic fields are of this type (it
    cannot be of a basic type as it would not be generic anymore) but
    method arguments or even generic parameters are also of this
    type. *)
and fieldTypeSignature =
  | GClass of classTypeSignature
  | GArray of typeSignature
  | GVariable of typeVariable
[@@deriving show]

type methodTypeSignature ={
  mts_formal_type_parameters : formalTypeParameter list;
  mts_type_signature : typeSignature list;
  mts_return_type : typeSignature option;
  mts_throws : throwsSignature list;
}
[@@deriving show]
end
module PJCode = struct

open PJBasics

type jconst = [
  | `ANull (* AConstNull  *)
  | `Int of int32
  | `Long of int64
  | `Float of float
  | `Double of float
  | `Byte of int (* BIPush *)
  | `Short of int
  | `String of jstr
  | `Class of object_type
]
[@@deriving show]
type jopcode =

  (* Access to a local variable *)
  | OpLoad of jvm_type * int
  | OpStore of jvm_type * int
  | OpIInc of int * int

  (* Stack permutation *)
  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap

  (* Constant loading / it corresponds to instructions *const* and ldc* *)
  | OpConst of jconst

  (* Arithmetic *)
  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

  (* Logic *)
  | OpIShl (* Use an I/L argument *)
  | OpLShl
  | OpIShr
  | OpLShr
  | OpIUShr
  | OpLUShr
  | OpIAnd
  | OpLAnd
  | OpIOr
  | OpLOr
  | OpIXor
  | OpLXor

  (* Conversion *)
  | OpI2L (* Use `I of [`L | `F  | `D] *)
  | OpI2F
  | OpI2D
  | OpL2I
  | OpL2F
  | OpL2D
  | OpF2I
  | OpF2L
  | OpF2D
  | OpD2I
  | OpD2L
  | OpD2F
  | OpI2B (* Those three are different *)
  | OpI2C
  | OpI2S

  | OpCmp of [`L | `FL | `FG | `DL | `DG]

  (* Conditional jump *)
  | OpIf of [`Eq | `Ne | `Lt | `Ge | `Gt | `Le | `Null | `NonNull] * int
  | OpIfCmp of [`IEq | `INe | `ILt | `IGe | `IGt | `ILe | `AEq | `ANe] * int

  (* Unconditional jump *)
  | OpGoto of int
  | OpJsr of int
  | OpRet of int
  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  (* Heap and static fields *)
  | OpNew of class_name
  | OpNewArray of value_type
  | OpAMultiNewArray of object_type * int (* ClassInfo, dims *)
  | OpCheckCast of object_type
  | OpInstanceOf of object_type
  | OpGetStatic of class_name * field_signature
  | OpPutStatic of class_name * field_signature
  | OpGetField of class_name * field_signature
  | OpPutField of class_name * field_signature
  | OpArrayLength
  | OpArrayLoad of jvm_array_type
  | OpArrayStore of jvm_array_type

  (* Method invocation and return *)
  | OpInvoke of [
    | `Virtual of object_type
    | `Special of class_name
    | `Static of class_name
    | `Interface of class_name
    ]
    * method_signature
  | OpReturn of jvm_return_type

  (* Exceptions and threads *)
  | OpThrow
  | OpMonitorEnter
  | OpMonitorExit

  (* Other *)
  | OpNop
  | OpBreakpoint
  | OpInvalid
[@@deriving show]

type jopcodes = jopcode array
[@@deriving show]

(* Exception handler. *)
type exception_handler = {
  e_start : int;
  e_end : int;
  e_handler : int;
  e_catch_type : class_name option
}
[@@deriving show]

type jcode = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : jopcodes;
  c_exc_tbl : exception_handler list;
  c_line_number_table : (int * int) list option;
  c_local_variable_table : (int * int * string * value_type * int) list option;
  c_local_variable_type_table : (int * int * string * PPJSignature.fieldTypeSignature * int) list option;
  c_stack_map_midp : stackmap list option;
  c_stack_map_java6 : stackmap list option;
  c_attributes : (string * string) list;
}
[@@deriving show]

end


module PJClass = struct

open PJBasics
open PJCode

type access = [
| `Default
| `Public
| `Private
| `Protected
]
[@@deriving show]

(** Generic attributes common to classes, fields and methods. *)
type attributes = {
  synthetic : bool;
  (** correspond to the attribute, not to the flag (cf. JVM Spec 1.5
      §4.2, §4.6, §4.7 and §4.8.7) *)
  deprecated : bool;
  other : (string * string) list
}
[@@deriving show]

(** visibility modifiers for annotations. An annotation may either be visible at
    run-time ([RTVisible]) or only present in the class file without being
    visible at run-time ([RTInvisible]).  (Note that there exists a third
    visibility at the Java source level, but as it corresponds to the
    source-only visibility they are not in the class file anymore.)  *)
type visibility = RTVisible | RTInvisible
[@@deriving show]


(** {2 Fields of classes and interfaces.} *)

(** Field kind *)
type field_kind =
  | NotFinal
  | Final
  | Volatile
[@@deriving show]


(** Fields of classes. *)
type class_field = {
  cf_signature : field_signature;
  cf_class_signature : class_field_signature;
  cf_generic_signature : PPJSignature.fieldTypeSignature option;
  cf_access: access;
  cf_static : bool;
  cf_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.6 and §4.8.7) *)
  cf_enum : bool;
  cf_kind : field_kind;
  cf_value : constant_value option;
  cf_transient : bool;
  cf_annotations : (PJBasics.annotation * visibility) list;
  cf_other_flags : int list;
  cf_attributes : attributes
}
[@@deriving show]

(** Fields of interfaces are implicitly [public], [static] and
    [final].*)
type interface_field = {
  if_signature : field_signature;
  if_class_signature : class_field_signature;
  if_generic_signature : PPJSignature.fieldTypeSignature option;
  if_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.6 and §4.8.7) *)
  if_value : constant_value option;
  (** a constant_value is not mandatory, especially as it can be
      initialized by the class initializer <clinit>. *)
  if_annotations: (annotation*visibility) list;
  if_other_flags : int list;
  if_attributes : attributes
}
[@@deriving show]

type any_field = | InterfaceField of interface_field | ClassField of class_field
[@@deriving show]


type 'a implementation =
  | Native
  | Java of 'a Lazy.t
let rec pp_implementation poly_a fmt x =
      Format.fprintf fmt "<x>";



type method_annotations = {
  ma_global: (annotation*visibility) list;
  (** annotations that are for the whole method. *)
  ma_parameters: (annotation*visibility) list list;
  (** [\[al1,al2\]] represents the annotations for the 2 parameters of the
      method, [al1] being the annotations for the first parameter and [al2] the
      annotations for the second parameter.  The length is smaller than the
      number of parameters of the method (excluding the receiver this).*)
}
[@@deriving show]

(* The final attribute has no meaning for a static method, but the JVM spec
   authorizes it anyway... *)
type 'a concrete_method = {
  cm_signature : method_signature;
  cm_class_method_signature : class_method_signature;
  cm_static : bool;
  cm_final : bool;
  cm_synchronized : bool;
  cm_strict : bool; (* Correspond to flag ACC_STRICT, which shows if we are in
                    FP-strict mod or not. *)
  cm_access: access;
  cm_generic_signature : PPJSignature.methodTypeSignature option;
  cm_bridge: bool;
  cm_varargs : bool;
  cm_synthetic : bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.7 and §4.8.7) *)
  cm_other_flags : int list;
  cm_exceptions : class_name list;
  cm_attributes : attributes;
  cm_annotations : method_annotations;
  cm_implementation : 'a implementation;
}
[@@deriving show]

(** An abstract method cannot be final, synchronized, strict or private. *)
type abstract_method = {
  am_signature : method_signature;
  am_class_method_signature : class_method_signature;
  am_access: [`Public | `Protected | `Default];
  am_generic_signature : PPJSignature.methodTypeSignature option;
  am_bridge: bool;
  am_varargs: bool;
  am_synthetic: bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.7 and §4.8.7) *)
  am_other_flags : int list;
  am_exceptions : class_name list;
  am_attributes : attributes;
  am_annotations : method_annotations;
  am_annotation_default : element_value option;
  (** If the method is in an annotation interface, then [am_annotation_default]
      may contains a default value for this method (annotation element). *)
}
[@@deriving show]

type 'a jmethod =
  | AbstractMethod of abstract_method
  | ConcreteMethod of 'a concrete_method
[@@deriving show]

type inner_class = {
  ic_class_name : class_name option;
  ic_outer_class_name : class_name option;
  ic_source_name : string option;
  ic_access : access;
  ic_static : bool;
  ic_final : bool;
  ic_synthetic: bool;
  ic_annotation: bool;
  (** [true] if and only if the class is an annotation (it should in this case
      be an interface) *)
  ic_enum: bool;
  ic_other_flags : int list;
  ic_type : [`ConcreteClass | `Abstract | `Interface]
}
[@@deriving show]

type 'a jclass = {
  c_name : class_name;
  c_version : version;
  c_access : [`Public | `Default];
  c_final : bool;
  c_abstract : bool;
  c_super_class : class_name option;
  c_generic_signature : PPJSignature.classSignature option;
  c_fields : class_field FieldMap.t;
  c_interfaces : class_name list;
  c_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  c_sourcefile : string option;
  c_deprecated : bool;
  c_enclosing_method : (class_name * method_signature option) option;
  (** introduced with Java 5 for local classes (defined in methods'
      code). The first element is innermost class that encloses the
      declaration of the current class. The second element is the
      method that encose this class definition. cf
      {{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS},
      paragraph 4.8.6.*)
  c_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  c_inner_classes : inner_class list;
  c_synthetic: bool;
  (** correspond to the flag ACC_SYNTHETIC, not to the Attribute
      (cf. JVM Spec 1.5 §4.2 and §4.8.7) *)
  c_enum: bool;
  c_annotations: (annotation*visibility) list;
  c_other_flags : int list;
  c_other_attributes : (string * string) list;
  c_methods : 'a jmethod MethodMap.t;
}
let rec pp_jclass poly_a fmt x =
      Format.fprintf fmt "{ @[<hov>";
      ((((((((((((((((((((Format.pp_print_string fmt
                             "Ex03.PJClass.c_name = ";
                           (pp_class_name fmt) x.c_name);
                          Format.fprintf fmt ";@ ";
                          Format.pp_print_string fmt "c_version = ";
                          (pp_version fmt) x.c_version);
                         Format.fprintf fmt ";@ ";
                         Format.pp_print_string fmt "c_access = ";
                         ((function
                           | `Public -> Format.pp_print_string fmt "`Public"
                           | `Default ->
                               Format.pp_print_string fmt "`Default"))
                           x.c_access);
                        Format.fprintf fmt ";@ ";
                        Format.pp_print_string fmt "c_final = ";
                        (Format.fprintf fmt "%B") x.c_final);
                       Format.fprintf fmt ";@ ";
                       Format.pp_print_string fmt "c_abstract = ";
                       (Format.fprintf fmt "%B") x.c_abstract);
                      Format.fprintf fmt ";@ ";
                      Format.pp_print_string fmt "c_super_class = ";
                      ((function
                        | None  -> Format.pp_print_string fmt "None"
                        | Some x ->
                            (Format.pp_print_string fmt "(Some ";
                             (pp_class_name fmt) x;
                             Format.pp_print_string fmt ")")))
                        x.c_super_class);
                     Format.fprintf fmt ";@ ";
                     Format.pp_print_string fmt "c_generic_signature = ";
                     ((function
                       | None  -> Format.pp_print_string fmt "None"
                       | Some x ->
                           (Format.pp_print_string fmt "(Some ";
                            (PPJSignature.pp_classSignature fmt) x;
                            Format.pp_print_string fmt ")")))
                       x.c_generic_signature);
                    Format.fprintf fmt ";@ ";
                    Format.pp_print_string fmt "c_fields = ";
                    (*let fields = get_fields ioc in

                    (FieldMap.pp (fun fmt  -> pp_class_field fmt) fmt)
                      x.c_fields);*)
                   Format.fprintf fmt ";@ ";
                   Format.pp_print_string fmt "c_interfaces = ";
                   ((fun x  ->
                       Format.fprintf fmt "[@[<hov>";
                       ignore
                         (List.fold_left
                            (fun sep  ->
                               fun x  ->
                                 if sep then Format.fprintf fmt ";@ ";
                                 (pp_class_name fmt) x;
                                 true) false x);
                       Format.fprintf fmt "@]]")) x.c_interfaces);
                  Format.fprintf fmt ";@ ";
                  Format.pp_print_string fmt "c_consts = ";
                  ((fun x  ->
                      Format.fprintf fmt "[|@[<hov>";
                      ignore
                        (Array.fold_left
                           (fun sep  ->
                              fun x  ->
                                if sep then Format.fprintf fmt ";@ ";
                                (pp_constant fmt) x;
                                true) false x);
                      Format.fprintf fmt "@]|]")) x.c_consts);
                 Format.fprintf fmt ";@ ";
                 Format.pp_print_string fmt "c_sourcefile = ";
                 ((function
                   | None  -> Format.pp_print_string fmt "None"
                   | Some x ->
                       (Format.pp_print_string fmt "(Some ";
                        (Format.fprintf fmt "%S") x;
                        Format.pp_print_string fmt ")"))) x.c_sourcefile);
                Format.fprintf fmt ";@ ";
                Format.pp_print_string fmt "c_deprecated = ";
                (Format.fprintf fmt "%B") x.c_deprecated);
               Format.fprintf fmt ";@ ";
               Format.pp_print_string fmt "c_enclosing_method = ";
               ((function
                 | None  -> Format.pp_print_string fmt "None"
                 | Some x ->
                     (Format.pp_print_string fmt "(Some ";
                      ((fun (a0,a1)  ->
                          Format.fprintf fmt "(@[<hov>";
                          ((pp_class_name fmt) a0;
                           Format.fprintf fmt ",@ ";
                           ((function
                             | None  -> Format.pp_print_string fmt "None"
                             | Some x ->
                                 (Format.pp_print_string fmt "(Some ";
                                  (pp_method_signature fmt) x;
                                  Format.pp_print_string fmt ")"))) a1);
                          Format.fprintf fmt "@])")) x;
                      Format.pp_print_string fmt ")"))) x.c_enclosing_method);
              Format.fprintf fmt ";@ ";
              Format.pp_print_string fmt "c_source_debug_extention = ";
              ((function
                | None  -> Format.pp_print_string fmt "None"
                | Some x ->
                    (Format.pp_print_string fmt "(Some ";
                     (Format.fprintf fmt "%S") x;
                     Format.pp_print_string fmt ")")))
                x.c_source_debug_extention);
             Format.fprintf fmt ";@ ";
             Format.pp_print_string fmt "c_inner_classes = ";
             ((fun x  ->
                 Format.fprintf fmt "[@[<hov>";
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ";@ ";
                           (pp_inner_class fmt) x;
                           true) false x);
                 Format.fprintf fmt "@]]")) x.c_inner_classes);
            Format.fprintf fmt ";@ ";
            Format.pp_print_string fmt "c_synthetic = ";
            (Format.fprintf fmt "%B") x.c_synthetic);
           Format.fprintf fmt ";@ ";
           Format.pp_print_string fmt "c_enum = ";
           (Format.fprintf fmt "%B") x.c_enum);
          Format.fprintf fmt ";@ ";
          Format.pp_print_string fmt "c_annotations = ";
          ((fun x  ->
              Format.fprintf fmt "[@[<hov>";
              ignore
                (List.fold_left
                   (fun sep  ->
                      fun x  ->
                        if sep then Format.fprintf fmt ";@ ";
                        ((fun (a0,a1)  ->
                            Format.fprintf fmt "(@[<hov>";
                            ((pp_annotation fmt) a0;
                             Format.fprintf fmt ",@ ";
                             (pp_visibility fmt) a1);
                            Format.fprintf fmt "@])")) x;
                        true) false x);
              Format.fprintf fmt "@]]")) x.c_annotations);
         Format.fprintf fmt ";@ ";
         Format.pp_print_string fmt "c_other_flags = ";
         ((fun x  ->
             Format.fprintf fmt "[@[<hov>";
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                       if sep then Format.fprintf fmt ";@ ";
                       (Format.fprintf fmt "%d") x;
                       true) false x);
             Format.fprintf fmt "@]]")) x.c_other_flags);
        Format.fprintf fmt ";@ ";
        Format.pp_print_string fmt "c_other_attributes = ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      ((fun (a0,a1)  ->
                          Format.fprintf fmt "(@[<hov>";
                          ((Format.fprintf fmt "%S") a0;
                           Format.fprintf fmt ",@ ";
                           (Format.fprintf fmt "%S") a1);
                          Format.fprintf fmt "@])")) x;
                      true) false x);
            Format.fprintf fmt "@]]")) x.c_other_attributes);
       Format.fprintf fmt ";@ ";
       Format.pp_print_string fmt "c_methods = "
       (*;
       (MethodMap.pp (fun fmt  -> pp_jmethod (fun fmt  -> poly_a fmt) fmt)
          fmt) x.c_methods*));
      Format.fprintf fmt "@] }"
    and show_jclass poly_a x = Format.asprintf "%a" (pp_jclass poly_a) x

type 'a jinterface = {
  i_name : class_name;
  i_version : version;
  i_access : [`Public | `Default];
  i_interfaces : class_name list;
  i_generic_signature : PPJSignature.classSignature option;
  i_consts : constant array; (** needed at least for unparsed/unknown attributes that might refer to the constant pool. *)
  i_sourcefile : string option;
  i_deprecated : bool;
  i_source_debug_extention : string option;
  (** Introduced in Java 5 for debugging purpose (no
      semantics defined)
      ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  i_inner_classes : inner_class list;
  i_initializer : 'a concrete_method option;
  (** the signature is <clinit>()V; and the method should be static  *)
  i_annotation: bool;
  (** [true] if and only if the interface is an annotation. *)
  i_annotations: (annotation*visibility) list;
  i_other_attributes : (string * string) list;
  i_other_flags : int list;
  i_fields : interface_field FieldMap.t;
  i_methods : abstract_method MethodMap.t;
}
    let rec pp_jinterface poly_a fmt x =
      Format.fprintf fmt "{ @[<hov>";
      (((((((((((((((((Format.pp_print_string fmt "Ex03.PJClass.i_name = ";
                       (pp_class_name fmt) x.i_name);
                      Format.fprintf fmt ";@ ";
                      Format.pp_print_string fmt "i_version = ";
                      (pp_version fmt) x.i_version);
                     Format.fprintf fmt ";@ ";
                     Format.pp_print_string fmt "i_access = ";
                     ((function
                       | `Public -> Format.pp_print_string fmt "`Public"
                       | `Default -> Format.pp_print_string fmt "`Default"))
                       x.i_access);
                    Format.fprintf fmt ";@ ";
                    Format.pp_print_string fmt "i_interfaces = ";
                    ((fun x  ->
                        Format.fprintf fmt "[@[<hov>";
                        ignore
                          (List.fold_left
                             (fun sep  ->
                                fun x  ->
                                  if sep then Format.fprintf fmt ";@ ";
                                  (pp_class_name fmt) x;
                                  true) false x);
                        Format.fprintf fmt "@]]")) x.i_interfaces);
                   Format.fprintf fmt ";@ ";
                   Format.pp_print_string fmt "i_generic_signature = ";
                   ((function
                     | None  -> Format.pp_print_string fmt "None"
                     | Some x ->
                         (Format.pp_print_string fmt "(Some ";
                          (PPJSignature.pp_classSignature fmt) x;
                          Format.pp_print_string fmt ")")))
                     x.i_generic_signature);
                  Format.fprintf fmt ";@ ";
                  Format.pp_print_string fmt "i_consts = ";
                  ((fun x  ->
                      Format.fprintf fmt "[|@[<hov>";
                      ignore
                        (Array.fold_left
                           (fun sep  ->
                              fun x  ->
                                if sep then Format.fprintf fmt ";@ ";
                                (pp_constant fmt) x;
                                true) false x);
                      Format.fprintf fmt "@]|]")) x.i_consts);
                 Format.fprintf fmt ";@ ";
                 Format.pp_print_string fmt "i_sourcefile = ";
                 ((function
                   | None  -> Format.pp_print_string fmt "None"
                   | Some x ->
                       (Format.pp_print_string fmt "(Some ";
                        (Format.fprintf fmt "%S") x;
                        Format.pp_print_string fmt ")"))) x.i_sourcefile);
                Format.fprintf fmt ";@ ";
                Format.pp_print_string fmt "i_deprecated = ";
                (Format.fprintf fmt "%B") x.i_deprecated);
               Format.fprintf fmt ";@ ";
               Format.pp_print_string fmt "i_source_debug_extention = ";
               ((function
                 | None  -> Format.pp_print_string fmt "None"
                 | Some x ->
                     (Format.pp_print_string fmt "(Some ";
                      (Format.fprintf fmt "%S") x;
                      Format.pp_print_string fmt ")")))
                 x.i_source_debug_extention);
              Format.fprintf fmt ";@ ";
              Format.pp_print_string fmt "i_inner_classes = ";
              ((fun x  ->
                  Format.fprintf fmt "[@[<hov>";
                  ignore
                    (List.fold_left
                       (fun sep  ->
                          fun x  ->
                            if sep then Format.fprintf fmt ";@ ";
                            (pp_inner_class fmt) x;
                            true) false x);
                  Format.fprintf fmt "@]]")) x.i_inner_classes);
             Format.fprintf fmt ";@ ";
             Format.pp_print_string fmt "i_initializer = ";
             ((function
               | None  -> Format.pp_print_string fmt "None"
               | Some x ->
                   (Format.pp_print_string fmt "(Some ";
                    (pp_concrete_method (fun fmt  -> poly_a fmt) fmt) x;
                    Format.pp_print_string fmt ")"))) x.i_initializer);
            Format.fprintf fmt ";@ ";
            Format.pp_print_string fmt "i_annotation = ";
            (Format.fprintf fmt "%B") x.i_annotation);
           Format.fprintf fmt ";@ ";
           Format.pp_print_string fmt "i_annotations = ";
           ((fun x  ->
               Format.fprintf fmt "[@[<hov>";
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt ";@ ";
                         ((fun (a0,a1)  ->
                             Format.fprintf fmt "(@[<hov>";
                             ((pp_annotation fmt) a0;
                              Format.fprintf fmt ",@ ";
                              (pp_visibility fmt) a1);
                             Format.fprintf fmt "@])")) x;
                         true) false x);
               Format.fprintf fmt "@]]")) x.i_annotations);
          Format.fprintf fmt ";@ ";
          Format.pp_print_string fmt "i_other_attributes = ";
          ((fun x  ->
              Format.fprintf fmt "[@[<hov>";
              ignore
                (List.fold_left
                   (fun sep  ->
                      fun x  ->
                        if sep then Format.fprintf fmt ";@ ";
                        ((fun (a0,a1)  ->
                            Format.fprintf fmt "(@[<hov>";
                            ((Format.fprintf fmt "%S") a0;
                             Format.fprintf fmt ",@ ";
                             (Format.fprintf fmt "%S") a1);
                            Format.fprintf fmt "@])")) x;
                        true) false x);
              Format.fprintf fmt "@]]")) x.i_other_attributes);
         Format.fprintf fmt ";@ ";
         Format.pp_print_string fmt "i_other_flags = ";
         ((fun x  ->
             Format.fprintf fmt "[@[<hov>";
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                       if sep then Format.fprintf fmt ";@ ";
                       (Format.fprintf fmt "%d") x;
                       true) false x);
             Format.fprintf fmt "@]]")) x.i_other_flags);
        Format.fprintf fmt ";@ ";
        Format.pp_print_string fmt "i_fields = ";
        ( (*FieldMap.pp (fun fmt  -> pp_interface_field fmt) fmt) x.i_fields *) ));
       Format.fprintf fmt ";@ ";
       Format.pp_print_string fmt "i_methods = ";
       (* (MethodMap.pp (fun fmt  -> pp_abstract_method fmt) fmt) x.i_methods *)
       ()
     );
      Format.fprintf fmt "@] }"
    and show_jinterface poly_a x =
      Format.asprintf "%a" (pp_jinterface poly_a) x
type 'a interface_or_class =
  | JInterface of 'a jinterface
  | JClass of 'a jclass
[@@deriving show]

end


module AA = struct
open Javalib_pack

let a =
  let aname = JBasics.make_cn "A" in

  let class_path = Javalib.class_path "./" in

  Printf.printf "get_class\n";

  let a = Javalib.get_class class_path aname in

  Javalib.JPrint.print_jasmin a stdout;

  Format.fprintf Format.std_formatter "%s\n" (PJClass.show_interface_or_class (fun a -> None) a);
  Javalib.JPrint.print_class a (fun a->Javalib.JPrint.jcode a)stdout;
  
end
