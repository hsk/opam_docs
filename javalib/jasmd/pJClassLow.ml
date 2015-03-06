open Javalib_pack
open JBasics
open PJBasics
open JClassLow

(*
(** Instruction. *)
type opcode =
  | OpNop
  | OpAConstNull
  | OpIConst of int32
  | OpLConst of int64
  | OpFConst of float
  | OpDConst of float
  | OpBIPush of int
  | OpSIPush of int
  | OpLdc1 of int
  | OpLdc1w of int
  | OpLdc2w of int

  | OpLoad of jvm_basic_type * int
  | OpALoad of int

  | OpArrayLoad of [`Int | other_num]
  | OpAALoad
  | OpBALoad
  | OpCALoad
  | OpSALoad

  | OpStore of jvm_basic_type * int
  | OpAStore of int

  | OpArrayStore of [`Int | other_num]
  | OpAAStore
  | OpBAStore
  | OpCAStore
  | OpSAStore

  | OpPop
  | OpPop2
  | OpDup
  | OpDupX1
  | OpDupX2
  | OpDup2
  | OpDup2X1
  | OpDup2X2
  | OpSwap

  | OpAdd of jvm_basic_type
  | OpSub of jvm_basic_type
  | OpMult of jvm_basic_type
  | OpDiv of jvm_basic_type
  | OpRem of jvm_basic_type
  | OpNeg of jvm_basic_type

  | OpIShl
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

  | OpIInc of int * int (** index, increment *)

  | OpI2L
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
  | OpI2B
  | OpI2C
  | OpI2S

  | OpLCmp
  | OpFCmpL
  | OpFCmpG
  | OpDCmpL
  | OpDCmpG
  | OpIfEq of int
  | OpIfNe of int
  | OpIfLt of int
  | OpIfGe of int
  | OpIfGt of int
  | OpIfLe of int
  | OpICmpEq of int
  | OpICmpNe of int
  | OpICmpLt of int
  | OpICmpGe of int
  | OpICmpGt of int
  | OpICmpLe of int
  | OpACmpEq of int
  | OpACmpNe of int
  | OpGoto of int
  | OpJsr of int
  | OpRet of int

  | OpTableSwitch of int * int32 * int32 * int array
  | OpLookupSwitch of int * (int32 * int) list

  | OpReturn of jvm_basic_type
  | OpAReturn
  | OpReturnVoid

  | OpGetStatic of int
  | OpPutStatic of int
  | OpGetField of int
  | OpPutField of int
  | OpInvokeVirtual of int
  | OpInvokeNonVirtual of int
  | OpInvokeStatic of int
  | OpInvokeInterface of int * int (** count *)

  | OpNew of int
  | OpNewArray of java_basic_type
  | OpANewArray of int
  | OpArrayLength
  | OpThrow
  | OpCheckCast of int
  | OpInstanceOf of int
  | OpMonitorEnter
  | OpMonitorExit
  | OpAMultiNewArray of int * int (** ClassInfo, dims *)
  | OpIfNull of int               (* offset *)
  | OpIfNonNull of int            (* offset *)
  | OpGotoW of int                (* offset *)
  | OpJsrW of int                 (* offset *)
  | OpBreakpoint                  (* should not be found *)
  (* | OpRetW of int *)
  | OpInvalid
      (* if [opcodes.(i) = OpInvalid] it means that there is an opcode
         that starts at position j, with j<i, an covers positions up
         to k, with k>=i. *)

type opcodes = opcode array

(** {2 Flags, attributes and low-level structure of class files.} *)

type common_flag = [
| `AccPublic
| `AccSynthetic
| `AccRFU of int (** The int is a mask. *)
| `AccFinal
]

type inner_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccInterface
| `AccAbstract
| `AccAnnotation
| `AccEnum
]

type field_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccVolatile
| `AccTransient
| `AccEnum
]

type method_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccSynchronized
| `AccBridge
| `AccVarArgs
| `AccNative
| `AccAbstract
| `AccStrict
]

type class_flag = [
| common_flag
| `AccAbstract
| `AccAnnotation
| `AccEnum
| `AccInterface
| `AccSuper
]

type access_flag = [
| common_flag
| `AccPrivate
| `AccProtected
| `AccStatic
| `AccSynchronized
| `AccVolatile
| `AccTransient
| `AccNative
| `AccInterface
| `AccAbstract
| `AccStrict
| `AccSuper
| `AccBridge
| `AccVarArgs
| `AccAnnotation
| `AccEnum
]


(** DFr : Addition for 1.6 stackmap. *)
type stackmap_frame =
  | SameFrame of int
  | SameLocals of int * verification_type
  | SameLocalsExtended of int * int * verification_type
  | ChopFrame of int * int
  | SameFrameExtended of int * int
  | AppendFrame of int * int * verification_type list
  | FullFrame of int * int * verification_type list * verification_type list

type code = {
  c_max_stack : int;
  c_max_locals : int;
  c_code : opcodes;
  c_exc_tbl : JCode.exception_handler list;
  c_attributes : attribute list;
}

and attribute =
  | AttributeSourceFile of string
  | AttributeConstant of constant_value
  | AttributeCode of code Lazy.t
  | AttributeExceptions of class_name list
  | AttributeInnerClasses of
      (class_name option * class_name option * string option
       * inner_flag list) list
  (** inner_class_info, outer_class_info, inner_name,
      inner_class_access_flags *)
  | AttributeSynthetic
  | AttributeLineNumberTable of (int * int) list
  | AttributeLocalVariableTable of (int * int * string * value_type * int) list
      (** start_pc, length, name, type, index *)
  | AttributeLocalVariableTypeTable of (int * int * string * JSignature.fieldTypeSignature * int) list
      (** (start_pc, length, name, type, index), LocalVariableTable for
          generics, described in the JVM Spec 1.5, §4.8.13 *)
  | AttributeDeprecated
  | AttributeStackMap of (int*(verification_type list)
        *(verification_type list)) list
  | AttributeSignature of string
      (** Introduced in Java 5 for generics
    ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}).*)
  | AttributeEnclosingMethod of (class_name * (string * descriptor) option)
      (** Introduced in Java 5 for local classes (classes
    defined in a method body)
    ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}).*)
  | AttributeSourceDebugExtension of string
      (** Introduced in Java 5 for debugging purpose (no
    semantics defined)
    ({{:http://java.sun.com/docs/books/jvms/second_edition/ClassFileFormat-Java5.pdf}JVMS}). *)
  | AttributeStackMapTable of stackmap_frame list
      (** DFr : Addition for 1.6 stackmap. *)
  | AttributeRuntimeVisibleAnnotations of annotation list
  | AttributeRuntimeInvisibleAnnotations of annotation list
  | AttributeRuntimeVisibleParameterAnnotations of annotation list list
  | AttributeRuntimeInvisibleParameterAnnotations of annotation list list
  | AttributeAnnotationDefault of element_value  (* cf. §4.8.19 of JVM Spec 5 *)
  | AttributeUnknown of string * string

type jfield = {
  f_name : string;
  f_descriptor : value_type;
  f_flags : field_flag list;
  f_attributes : attribute list
}

type jmethod = {
  m_name : string;
  m_descriptor : method_descriptor;
  m_flags : method_flag list;
  m_attributes : attribute list
}

type jclass = {
  j_name : class_name;
  j_super : class_name option;
  j_interfaces : class_name list;
  j_consts : constant array;
  j_flags : class_flag list;
  j_fields : jfield list;
  j_methods : jmethod list;
  j_attributes : attribute list;
  j_version : version;
}
*)

let rec pp_opcode fmt = function
  | OpNop  -> Format.pp_print_string fmt "JClassLow.OpNop"
  | OpAConstNull  -> Format.pp_print_string fmt "JClassLow.OpAConstNull"
  | OpIConst a0 ->
      Format.fprintf fmt "(@[<hov2>JClassLow.OpIConst@ ";
      Format.fprintf fmt "%ldl" a0;
      Format.fprintf fmt "@])"
  | OpLConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpLConst@ ";
       (Format.fprintf fmt "%LdL") a0;
       Format.fprintf fmt "@])")
  | OpFConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpFConst@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | OpDConst a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpDConst@ ";
       (Format.fprintf fmt "%F") a0;
       Format.fprintf fmt "@])")
  | OpBIPush a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpBIPush@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpSIPush a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpSIPush@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLdc1 a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpLdc1@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLdc1w a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpLdc1w@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLdc2w a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpLdc2w@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpLoad (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpLoad (@,";
       ((pp_jvm_basic_type fmt) a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpALoad a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpALoad@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpArrayLoad a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpArrayLoad@ ";
       ((function
         | `Int -> Format.pp_print_string fmt "`Int"
         | #other_num as x -> (pp_other_num fmt) x)) a0;
       Format.fprintf fmt "@])")
  | OpAALoad  -> Format.pp_print_string fmt "JClassLow.OpAALoad"
  | OpBALoad  -> Format.pp_print_string fmt "JClassLow.OpBALoad"
  | OpCALoad  -> Format.pp_print_string fmt "JClassLow.OpCALoad"
  | OpSALoad  -> Format.pp_print_string fmt "JClassLow.OpSALoad"
  | OpStore (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpStore (@,";
       ((pp_jvm_basic_type fmt) a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpAStore a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpAStore@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpArrayStore a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpArrayStore@ ";
       ((function
         | `Int -> Format.pp_print_string fmt "`Int"
         | #other_num as x -> (pp_other_num fmt) x)) a0;
       Format.fprintf fmt "@])")
  | OpAAStore  -> Format.pp_print_string fmt "JClassLow.OpAAStore"
  | OpBAStore  -> Format.pp_print_string fmt "JClassLow.OpBAStore"
  | OpCAStore  -> Format.pp_print_string fmt "JClassLow.OpCAStore"
  | OpSAStore  -> Format.pp_print_string fmt "JClassLow.OpSAStore"
  | OpPop  -> Format.pp_print_string fmt "JClassLow.OpPop"
  | OpPop2  -> Format.pp_print_string fmt "JClassLow.OpPop2"
  | OpDup  -> Format.pp_print_string fmt "JClassLow.OpDup"
  | OpDupX1  -> Format.pp_print_string fmt "JClassLow.OpDupX1"
  | OpDupX2  -> Format.pp_print_string fmt "JClassLow.OpDupX2"
  | OpDup2  -> Format.pp_print_string fmt "JClassLow.OpDup2"
  | OpDup2X1  -> Format.pp_print_string fmt "JClassLow.OpDup2X1"
  | OpDup2X2  -> Format.pp_print_string fmt "JClassLow.OpDup2X2"
  | OpSwap  -> Format.pp_print_string fmt "JClassLow.OpSwap"
  | OpAdd a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpAdd@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpSub a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpSub@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpMult a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpMult@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpDiv a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpDiv@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpRem a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpRem@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpNeg a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpNeg@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpIShl  -> Format.pp_print_string fmt "JClassLow.OpIShl"
  | OpLShl  -> Format.pp_print_string fmt "JClassLow.OpLShl"
  | OpIShr  -> Format.pp_print_string fmt "JClassLow.OpIShr"
  | OpLShr  -> Format.pp_print_string fmt "JClassLow.OpLShr"
  | OpIUShr  -> Format.pp_print_string fmt "JClassLow.OpIUShr"
  | OpLUShr  -> Format.pp_print_string fmt "JClassLow.OpLUShr"
  | OpIAnd  -> Format.pp_print_string fmt "JClassLow.OpIAnd"
  | OpLAnd  -> Format.pp_print_string fmt "JClassLow.OpLAnd"
  | OpIOr  -> Format.pp_print_string fmt "JClassLow.OpIOr"
  | OpLOr  -> Format.pp_print_string fmt "JClassLow.OpLOr"
  | OpIXor  -> Format.pp_print_string fmt "JClassLow.OpIXor"
  | OpLXor  -> Format.pp_print_string fmt "JClassLow.OpLXor"
  | OpIInc (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpIInc (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpI2L  -> Format.pp_print_string fmt "JClassLow.OpI2L"
  | OpI2F  -> Format.pp_print_string fmt "JClassLow.OpI2F"
  | OpI2D  -> Format.pp_print_string fmt "JClassLow.OpI2D"
  | OpL2I  -> Format.pp_print_string fmt "JClassLow.OpL2I"
  | OpL2F  -> Format.pp_print_string fmt "JClassLow.OpL2F"
  | OpL2D  -> Format.pp_print_string fmt "JClassLow.OpL2D"
  | OpF2I  -> Format.pp_print_string fmt "JClassLow.OpF2I"
  | OpF2L  -> Format.pp_print_string fmt "JClassLow.OpF2L"
  | OpF2D  -> Format.pp_print_string fmt "JClassLow.OpF2D"
  | OpD2I  -> Format.pp_print_string fmt "JClassLow.OpD2I"
  | OpD2L  -> Format.pp_print_string fmt "JClassLow.OpD2L"
  | OpD2F  -> Format.pp_print_string fmt "JClassLow.OpD2F"
  | OpI2B  -> Format.pp_print_string fmt "JClassLow.OpI2B"
  | OpI2C  -> Format.pp_print_string fmt "JClassLow.OpI2C"
  | OpI2S  -> Format.pp_print_string fmt "JClassLow.OpI2S"
  | OpLCmp  -> Format.pp_print_string fmt "JClassLow.OpLCmp"
  | OpFCmpL  -> Format.pp_print_string fmt "JClassLow.OpFCmpL"
  | OpFCmpG  -> Format.pp_print_string fmt "JClassLow.OpFCmpG"
  | OpDCmpL  -> Format.pp_print_string fmt "JClassLow.OpDCmpL"
  | OpDCmpG  -> Format.pp_print_string fmt "JClassLow.OpDCmpG"
  | OpIfEq a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfEq@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfNe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfNe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfLt a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfLt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfGe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfGe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfGt a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfGt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfLe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfLe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpEq a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpICmpEq@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpNe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpICmpNe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpLt a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpICmpLt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpGe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpICmpGe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpGt a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpICmpGt@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpICmpLe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpICmpLe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpACmpEq a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpACmpEq@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpACmpNe a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpACmpNe@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpGoto a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpGoto@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpJsr a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpJsr@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpRet a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpRet@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpTableSwitch (a0,a1,a2,a3) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpTableSwitch (@,";
       ((((Format.fprintf fmt "%d") a0;
          Format.fprintf fmt ",@ ";
          (Format.fprintf fmt "%ldl") a1);
         Format.fprintf fmt ",@ ";
         (Format.fprintf fmt "%ldl") a2);
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[|@[<hov>";
            ignore
              (Array.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (Format.fprintf fmt "%d") x;
                      true) false x);
            Format.fprintf fmt "@]|]")) a3);
       Format.fprintf fmt "@])")
  | OpLookupSwitch (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpLookupSwitch (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      ((fun (a0,a1)  ->
                          Format.fprintf fmt "(@[<hov>";
                          ((Format.fprintf fmt "%ldl") a0;
                           Format.fprintf fmt ",@ ";
                           (Format.fprintf fmt "%d") a1);
                          Format.fprintf fmt "@])")) x;
                      true) false x);
            Format.fprintf fmt "@]]")) a1);
       Format.fprintf fmt "@])")
  | OpReturn a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpReturn@ ";
       (pp_jvm_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpAReturn  -> Format.pp_print_string fmt "JClassLow.OpAReturn"
  | OpReturnVoid  -> Format.pp_print_string fmt "JClassLow.OpReturnVoid"
  | OpGetStatic a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpGetStatic@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpPutStatic a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpPutStatic@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpGetField a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpGetField@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpPutField a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpPutField@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeVirtual a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpInvokeVirtual@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeNonVirtual a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpInvokeNonVirtual@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeStatic a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpInvokeStatic@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInvokeInterface (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpInvokeInterface (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpNew a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpNew@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpNewArray a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpNewArray@ ";
       (pp_java_basic_type fmt) a0;
       Format.fprintf fmt "@])")
  | OpANewArray a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpANewArray@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpArrayLength  -> Format.pp_print_string fmt "JClassLow.OpArrayLength"
  | OpThrow  -> Format.pp_print_string fmt "JClassLow.OpThrow"
  | OpCheckCast a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpCheckCast@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpInstanceOf a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpInstanceOf@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpMonitorEnter  -> Format.pp_print_string fmt "JClassLow.OpMonitorEnter"
  | OpMonitorExit  -> Format.pp_print_string fmt "JClassLow.OpMonitorExit"
  | OpAMultiNewArray (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.OpAMultiNewArray (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | OpIfNull a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfNull@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpIfNonNull a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpIfNonNull@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpGotoW a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpGotoW@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpJsrW a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.OpJsrW@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | OpBreakpoint  -> Format.pp_print_string fmt "JClassLow.OpBreakpoint"
  | OpInvalid  -> Format.pp_print_string fmt "JClassLow.OpInvalid"
and show_opcode x = Format.asprintf "%a" pp_opcode x

let rec pp_opcodes fmt x =
  Format.fprintf fmt "[|@[<hov>";
  ignore
    (Array.fold_left
       (fun sep  ->
          fun x  ->
            if sep then Format.fprintf fmt ";@ "; (pp_opcode fmt) x; true)
       false x);
  Format.fprintf fmt "@]|]"
and show_opcodes x = Format.asprintf "%a" pp_opcodes x

let rec pp_common_flag fmt =
  function
  | `AccPublic -> Format.pp_print_string fmt "`AccPublic"
  | `AccSynthetic -> Format.pp_print_string fmt "`AccSynthetic"
  | `AccRFU x ->
      (Format.fprintf fmt "`AccRFU (@[<hov>";
       (Format.fprintf fmt "%d") x;
       Format.fprintf fmt "@])")
  | `AccFinal -> Format.pp_print_string fmt "`AccFinal"
and show_common_flag x = Format.asprintf "%a" pp_common_flag x

let rec pp_inner_flag fmt =
  function
  | #common_flag as x -> (pp_common_flag fmt) x
  | `AccPrivate -> Format.pp_print_string fmt "`AccPrivate"
  | `AccProtected -> Format.pp_print_string fmt "`AccProtected"
  | `AccStatic -> Format.pp_print_string fmt "`AccStatic"
  | `AccInterface -> Format.pp_print_string fmt "`AccInterface"
  | `AccAbstract -> Format.pp_print_string fmt "`AccAbstract"
  | `AccAnnotation -> Format.pp_print_string fmt "`AccAnnotation"
  | `AccEnum -> Format.pp_print_string fmt "`AccEnum"
and show_inner_flag x = Format.asprintf "%a" pp_inner_flag x

let rec pp_field_flag fmt =
  function
  | #common_flag as x -> (pp_common_flag fmt) x
  | `AccPrivate -> Format.pp_print_string fmt "`AccPrivate"
  | `AccProtected -> Format.pp_print_string fmt "`AccProtected"
  | `AccStatic -> Format.pp_print_string fmt "`AccStatic"
  | `AccVolatile -> Format.pp_print_string fmt "`AccVolatile"
  | `AccTransient -> Format.pp_print_string fmt "`AccTransient"
  | `AccEnum -> Format.pp_print_string fmt "`AccEnum"
and show_field_flag x = Format.asprintf "%a" pp_field_flag x

let rec pp_method_flag fmt =
  function
  | #common_flag as x -> (pp_common_flag fmt) x
  | `AccPrivate -> Format.pp_print_string fmt "`AccPrivate"
  | `AccProtected -> Format.pp_print_string fmt "`AccProtected"
  | `AccStatic -> Format.pp_print_string fmt "`AccStatic"
  | `AccSynchronized -> Format.pp_print_string fmt "`AccSynchronized"
  | `AccBridge -> Format.pp_print_string fmt "`AccBridge"
  | `AccVarArgs -> Format.pp_print_string fmt "`AccVarArgs"
  | `AccNative -> Format.pp_print_string fmt "`AccNative"
  | `AccAbstract -> Format.pp_print_string fmt "`AccAbstract"
  | `AccStrict -> Format.pp_print_string fmt "`AccStrict"
and show_method_flag x = Format.asprintf "%a" pp_method_flag x

let rec pp_class_flag fmt =
  function
  | #common_flag as x -> (pp_common_flag fmt) x
  | `AccAbstract -> Format.pp_print_string fmt "`AccAbstract"
  | `AccAnnotation -> Format.pp_print_string fmt "`AccAnnotation"
  | `AccEnum -> Format.pp_print_string fmt "`AccEnum"
  | `AccInterface -> Format.pp_print_string fmt "`AccInterface"
  | `AccSuper -> Format.pp_print_string fmt "`AccSuper"
and show_class_flag x = Format.asprintf "%a" pp_class_flag x

let rec pp_access_flag fmt =
  function
  | #common_flag as x -> (pp_common_flag fmt) x
  | `AccPrivate -> Format.pp_print_string fmt "`AccPrivate"
  | `AccProtected -> Format.pp_print_string fmt "`AccProtected"
  | `AccStatic -> Format.pp_print_string fmt "`AccStatic"
  | `AccSynchronized -> Format.pp_print_string fmt "`AccSynchronized"
  | `AccVolatile -> Format.pp_print_string fmt "`AccVolatile"
  | `AccTransient -> Format.pp_print_string fmt "`AccTransient"
  | `AccNative -> Format.pp_print_string fmt "`AccNative"
  | `AccInterface -> Format.pp_print_string fmt "`AccInterface"
  | `AccAbstract -> Format.pp_print_string fmt "`AccAbstract"
  | `AccStrict -> Format.pp_print_string fmt "`AccStrict"
  | `AccSuper -> Format.pp_print_string fmt "`AccSuper"
  | `AccBridge -> Format.pp_print_string fmt "`AccBridge"
  | `AccVarArgs -> Format.pp_print_string fmt "`AccVarArgs"
  | `AccAnnotation -> Format.pp_print_string fmt "`AccAnnotation"
  | `AccEnum -> Format.pp_print_string fmt "`AccEnum"
and show_access_flag x = Format.asprintf "%a" pp_access_flag x

let rec pp_stackmap_frame fmt =
  function
  | SameFrame a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.SameFrame@ ";
       (Format.fprintf fmt "%d") a0;
       Format.fprintf fmt "@])")
  | SameLocals (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.SameLocals (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (pp_verification_type fmt) a1);
       Format.fprintf fmt "@])")
  | SameLocalsExtended (a0,a1,a2) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.SameLocalsExtended (@,";
       (((Format.fprintf fmt "%d") a0;
         Format.fprintf fmt ",@ ";
         (Format.fprintf fmt "%d") a1);
        Format.fprintf fmt ",@ ";
        (pp_verification_type fmt) a2);
       Format.fprintf fmt "@])")
  | ChopFrame (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.ChopFrame (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | SameFrameExtended (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.SameFrameExtended (@,";
       ((Format.fprintf fmt "%d") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%d") a1);
       Format.fprintf fmt "@])")
  | AppendFrame (a0,a1,a2) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.AppendFrame (@,";
       (((Format.fprintf fmt "%d") a0;
         Format.fprintf fmt ",@ ";
         (Format.fprintf fmt "%d") a1);
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_verification_type fmt) x;
                      true) false x);
            Format.fprintf fmt "@]]")) a2);
       Format.fprintf fmt "@])")
  | FullFrame (a0,a1,a2,a3) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.FullFrame (@,";
       ((((Format.fprintf fmt "%d") a0;
          Format.fprintf fmt ",@ ";
          (Format.fprintf fmt "%d") a1);
         Format.fprintf fmt ",@ ";
         ((fun x  ->
             Format.fprintf fmt "[@[<hov>";
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                       if sep then Format.fprintf fmt ";@ ";
                       (pp_verification_type fmt) x;
                       true) false x);
             Format.fprintf fmt "@]]")) a2);
        Format.fprintf fmt ",@ ";
        ((fun x  ->
            Format.fprintf fmt "[@[<hov>";
            ignore
              (List.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_verification_type fmt) x;
                      true) false x);
            Format.fprintf fmt "@]]")) a3);
       Format.fprintf fmt "@])")
and show_stackmap_frame x = Format.asprintf "%a" pp_stackmap_frame x

let rec pp_code fmt x =
  Format.fprintf fmt "{ @[<hov>";
  (((((Format.pp_print_string fmt "JClassLow.c_max_stack = ";
       (Format.fprintf fmt "%d") x.c_max_stack);
      Format.fprintf fmt ";@ ";
      Format.pp_print_string fmt "c_max_locals = ";
      (Format.fprintf fmt "%d") x.c_max_locals);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "c_code = ";
     (pp_opcodes fmt) x.c_code);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "c_exc_tbl = ";
    ((fun x  ->
        Format.fprintf fmt "[@[<hov>";
        ignore
          (List.fold_left
             (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt ";@ ";
                  (PJCode.pp_exception_handler fmt) x;
                  true) false x);
        Format.fprintf fmt "@]]")) x.c_exc_tbl);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "c_attributes = ";
   ((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left
            (fun sep  ->
               fun x  ->
                 if sep then Format.fprintf fmt ";@ ";
                 (pp_attribute fmt) x;
                 true) false x);
       Format.fprintf fmt "@]]")) x.c_attributes);
  Format.fprintf fmt "@] }"
and show_code x = Format.asprintf "%a" pp_code x
and pp_attribute fmt =
  function
  | AttributeSourceFile a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeSourceFile@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
  | AttributeConstant a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeConstant@ ";
       (pp_constant_value fmt) a0;
       Format.fprintf fmt "@])")
  | AttributeCode a0 ->
      Format.fprintf fmt "(@[<hov2>JClassLow.AttributeCode@ ";
      Format.fprintf fmt "lazy(%a)@ " pp_code (Lazy.force a0);
      Format.fprintf fmt "@])"
  | AttributeExceptions a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeExceptions@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_class_name fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeInnerClasses a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeInnerClasses@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun (a0,a1,a2,a3)  ->
                         Format.fprintf fmt "(@[<hov>";
                         (((((function
                              | None  -> Format.pp_print_string fmt "None"
                              | Some x ->
                                  (Format.pp_print_string fmt "(Some ";
                                   (pp_class_name fmt) x;
                                   Format.pp_print_string fmt ")"))) a0;
                            Format.fprintf fmt ",@ ";
                            ((function
                              | None  -> Format.pp_print_string fmt "None"
                              | Some x ->
                                  (Format.pp_print_string fmt "(Some ";
                                   (pp_class_name fmt) x;
                                   Format.pp_print_string fmt ")"))) a1);
                           Format.fprintf fmt ",@ ";
                           ((function
                             | None  -> Format.pp_print_string fmt "None"
                             | Some x ->
                                 (Format.pp_print_string fmt "(Some ";
                                  (Format.fprintf fmt "%S") x;
                                  Format.pp_print_string fmt ")"))) a2);
                          Format.fprintf fmt ",@ ";
                          ((fun x  ->
                              Format.fprintf fmt "[@[<hov>";
                              ignore
                                (List.fold_left
                                   (fun sep  ->
                                      fun x  ->
                                        if sep then Format.fprintf fmt ";@ ";
                                        (pp_inner_flag fmt) x;
                                        true) false x);
                              Format.fprintf fmt "@]]")) a3);
                         Format.fprintf fmt "@])")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeSynthetic  ->
      Format.pp_print_string fmt "JClassLow.AttributeSynthetic"
  | AttributeLineNumberTable a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeLineNumberTable@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun (a0,a1)  ->
                         Format.fprintf fmt "(@[<hov>";
                         ((Format.fprintf fmt "%d") a0;
                          Format.fprintf fmt ",@ ";
                          (Format.fprintf fmt "%d") a1);
                         Format.fprintf fmt "@])")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeLocalVariableTable a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeLocalVariableTable@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun (a0,a1,a2,a3,a4)  ->
                         Format.fprintf fmt "(@[<hov>";
                         (((((Format.fprintf fmt "%d") a0;
                             Format.fprintf fmt ",@ ";
                             (Format.fprintf fmt "%d") a1);
                            Format.fprintf fmt ",@ ";
                            (Format.fprintf fmt "%S") a2);
                           Format.fprintf fmt ",@ ";
                           (pp_value_type fmt) a3);
                          Format.fprintf fmt ",@ ";
                          (Format.fprintf fmt "%d") a4);
                         Format.fprintf fmt "@])")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeLocalVariableTypeTable a0 ->
      (Format.fprintf fmt
         "(@[<hov2>JClassLow.AttributeLocalVariableTypeTable@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun (a0,a1,a2,a3,a4)  ->
                         Format.fprintf fmt "(@[<hov>";
                         (((((Format.fprintf fmt "%d") a0;
                             Format.fprintf fmt ",@ ";
                             (Format.fprintf fmt "%d") a1);
                            Format.fprintf fmt ",@ ";
                            (Format.fprintf fmt "%S") a2);
                           Format.fprintf fmt ",@ ";
                           (PJSignature.pp_fieldTypeSignature fmt) a3);
                          Format.fprintf fmt ",@ ";
                          (Format.fprintf fmt "%d") a4);
                         Format.fprintf fmt "@])")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeDeprecated  ->
      Format.pp_print_string fmt "JClassLow.AttributeDeprecated"
  | AttributeStackMap a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeStackMap@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun (a0,a1,a2)  ->
                         Format.fprintf fmt "(@[<hov>";
                         (((Format.fprintf fmt "%d") a0;
                           Format.fprintf fmt ",@ ";
                           ((fun x  ->
                               Format.fprintf fmt "[@[<hov>";
                               ignore
                                 (List.fold_left
                                    (fun sep  ->
                                       fun x  ->
                                         if sep then Format.fprintf fmt ";@ ";
                                         (pp_verification_type fmt) x;
                                         true) false x);
                               Format.fprintf fmt "@]]")) a1);
                          Format.fprintf fmt ",@ ";
                          ((fun x  ->
                              Format.fprintf fmt "[@[<hov>";
                              ignore
                                (List.fold_left
                                   (fun sep  ->
                                      fun x  ->
                                        if sep then Format.fprintf fmt ";@ ";
                                        (pp_verification_type fmt) x;
                                        true) false x);
                              Format.fprintf fmt "@]]")) a2);
                         Format.fprintf fmt "@])")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeSignature a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeSignature@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
  | AttributeEnclosingMethod a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeEnclosingMethod@ ";
       ((fun (a0,a1)  ->
           Format.fprintf fmt "(@[<hov>";
           ((pp_class_name fmt) a0;
            Format.fprintf fmt ",@ ";
            ((function
              | None  -> Format.pp_print_string fmt "None"
              | Some x ->
                  (Format.pp_print_string fmt "(Some ";
                   ((fun (a0,a1)  ->
                       Format.fprintf fmt "(@[<hov>";
                       ((Format.fprintf fmt "%S") a0;
                        Format.fprintf fmt ",@ ";
                        (pp_descriptor fmt) a1);
                       Format.fprintf fmt "@])")) x;
                   Format.pp_print_string fmt ")"))) a1);
           Format.fprintf fmt "@])")) a0;
       Format.fprintf fmt "@])")
  | AttributeSourceDebugExtension a0 ->
      (Format.fprintf fmt
         "(@[<hov2>JClassLow.AttributeSourceDebugExtension@ ";
       (Format.fprintf fmt "%S") a0;
       Format.fprintf fmt "@])")
  | AttributeStackMapTable a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeStackMapTable@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_stackmap_frame fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeRuntimeVisibleAnnotations a0 ->
      (Format.fprintf fmt
         "(@[<hov2>JClassLow.AttributeRuntimeVisibleAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_annotation fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeRuntimeInvisibleAnnotations a0 ->
      (Format.fprintf fmt
         "(@[<hov2>JClassLow.AttributeRuntimeInvisibleAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_annotation fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeRuntimeVisibleParameterAnnotations a0 ->
      (Format.fprintf fmt
         "(@[<hov2>JClassLow.AttributeRuntimeVisibleParameterAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun x  ->
                         Format.fprintf fmt "[@[<hov>";
                         ignore
                           (List.fold_left
                              (fun sep  ->
                                 fun x  ->
                                   if sep then Format.fprintf fmt ";@ ";
                                   (pp_annotation fmt) x;
                                   true) false x);
                         Format.fprintf fmt "@]]")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeRuntimeInvisibleParameterAnnotations a0 ->
      (Format.fprintf fmt
         "(@[<hov2>JClassLow.AttributeRuntimeInvisibleParameterAnnotations@ ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     ((fun x  ->
                         Format.fprintf fmt "[@[<hov>";
                         ignore
                           (List.fold_left
                              (fun sep  ->
                                 fun x  ->
                                   if sep then Format.fprintf fmt ";@ ";
                                   (pp_annotation fmt) x;
                                   true) false x);
                         Format.fprintf fmt "@]]")) x;
                     true) false x);
           Format.fprintf fmt "@]]")) a0;
       Format.fprintf fmt "@])")
  | AttributeAnnotationDefault a0 ->
      (Format.fprintf fmt "(@[<hov2>JClassLow.AttributeAnnotationDefault@ ";
       (pp_element_value fmt) a0;
       Format.fprintf fmt "@])")
  | AttributeUnknown (a0,a1) ->
      (Format.fprintf fmt "@[<hov2>JClassLow.AttributeUnknown (@,";
       ((Format.fprintf fmt "%S") a0;
        Format.fprintf fmt ",@ ";
        (Format.fprintf fmt "%S") a1);
       Format.fprintf fmt "@])")
and show_attribute x = Format.asprintf "%a" pp_attribute x

let rec pp_jfield fmt x =
  Format.fprintf fmt "{ @[<hov>";
  ((((Format.pp_print_string fmt "JClassLow.f_name = ";
      (Format.fprintf fmt "%S") x.f_name);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "f_descriptor = ";
     (pp_value_type fmt) x.f_descriptor);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "f_flags = ";
    ((fun x  ->
        Format.fprintf fmt "[@[<hov>";
        ignore
          (List.fold_left
             (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt ";@ ";
                  (pp_field_flag fmt) x;
                  true) false x);
        Format.fprintf fmt "@]]")) x.f_flags);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "f_attributes = ";
   ((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left
            (fun sep  ->
               fun x  ->
                 if sep then Format.fprintf fmt ";@ ";
                 (pp_attribute fmt) x;
                 true) false x);
       Format.fprintf fmt "@]]")) x.f_attributes);
  Format.fprintf fmt "@] }"
and show_jfield x = Format.asprintf "%a" pp_jfield x

let rec pp_jmethod fmt x =
  Format.fprintf fmt "{ @[<hov>";
  ((((Format.pp_print_string fmt "JClassLow.m_name = ";
      (Format.fprintf fmt "%S") x.m_name);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "m_descriptor = ";
     (pp_method_descriptor fmt) x.m_descriptor);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "m_flags = ";
    ((fun x  ->
        Format.fprintf fmt "[@[<hov>";
        ignore
          (List.fold_left
             (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt ";@ ";
                  (pp_method_flag fmt) x;
                  true) false x);
        Format.fprintf fmt "@]]")) x.m_flags);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "m_attributes = ";
   ((fun x  ->
       Format.fprintf fmt "[@[<hov>";
       ignore
         (List.fold_left
            (fun sep  ->
               fun x  ->
                 if sep then Format.fprintf fmt ";@ ";
                 (pp_attribute fmt) x;
                 true) false x);
       Format.fprintf fmt "@]]")) x.m_attributes);
  Format.fprintf fmt "@] }"
and show_jmethod x = Format.asprintf "%a" pp_jmethod x

let rec pp_jclass fmt x =
  Format.fprintf fmt "{ @[<hov>";
  (((((((((Format.pp_print_string fmt "JClassLow.j_name = ";
           (pp_class_name fmt) x.j_name);
          Format.fprintf fmt ";@ ";
          Format.pp_print_string fmt "j_super = ";
          ((function
            | None  -> Format.pp_print_string fmt "None"
            | Some x ->
                (Format.pp_print_string fmt "(Some ";
                 (pp_class_name fmt) x;
                 Format.pp_print_string fmt ")"))) x.j_super);
         Format.fprintf fmt ";@ ";
         Format.pp_print_string fmt "j_interfaces = ";
         ((fun x  ->
             Format.fprintf fmt "[@[<hov>";
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                       if sep then Format.fprintf fmt ";@ ";
                       (pp_class_name fmt) x;
                       true) false x);
             Format.fprintf fmt "@]]")) x.j_interfaces);
        Format.fprintf fmt ";@ ";
        Format.pp_print_string fmt "j_consts = ";
        ((fun x  ->
            Format.fprintf fmt "[|@[<hov>";
            ignore
              (Array.fold_left
                 (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@ ";
                      (pp_constant fmt) x;
                      true) false x);
            Format.fprintf fmt "@]|]")) x.j_consts);
       Format.fprintf fmt ";@ ";
       Format.pp_print_string fmt "j_flags = ";
       ((fun x  ->
           Format.fprintf fmt "[@[<hov>";
           ignore
             (List.fold_left
                (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt ";@ ";
                     (pp_class_flag fmt) x;
                     true) false x);
           Format.fprintf fmt "@]]")) x.j_flags);
      Format.fprintf fmt ";@ ";
      Format.pp_print_string fmt "j_fields = ";
      ((fun x  ->
          Format.fprintf fmt "[@[<hov>";
          ignore
            (List.fold_left
               (fun sep  ->
                  fun x  ->
                    if sep then Format.fprintf fmt ";@ ";
                    (pp_jfield fmt) x;
                    true) false x);
          Format.fprintf fmt "@]]")) x.j_fields);
     Format.fprintf fmt ";@ ";
     Format.pp_print_string fmt "j_methods = ";
     ((fun x  ->
         Format.fprintf fmt "[@[<hov>";
         ignore
           (List.fold_left
              (fun sep  ->
                 fun x  ->
                   if sep then Format.fprintf fmt ";@ ";
                   (pp_jmethod fmt) x;
                   true) false x);
         Format.fprintf fmt "@]]")) x.j_methods);
    Format.fprintf fmt ";@ ";
    Format.pp_print_string fmt "j_attributes = ";
    ((fun x  ->
        Format.fprintf fmt "[@[<hov>";
        ignore
          (List.fold_left
             (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt ";@ ";
                  (pp_attribute fmt) x;
                  true) false x);
        Format.fprintf fmt "@]]")) x.j_attributes);
   Format.fprintf fmt ";@ ";
   Format.pp_print_string fmt "j_version = ";
   (pp_version fmt) x.j_version);
  Format.fprintf fmt "@] }"
and show_jclass x = Format.asprintf "%a" pp_jclass x

let pp = pp_jclass
let show = show_jclass
