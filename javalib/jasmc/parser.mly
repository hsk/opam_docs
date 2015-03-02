%{
open Javalib_pack
open Javalib

let sourcefile = ref None

let replace_dot s =
  let s = String.copy s in
    for i = 0 to String.length s - 1 do
      if s.[i] = '/' then s.[i] <- '.'
    done;
    s

let cn = ref (JBasics.make_cn "_")

let cf_access a =
  if List.mem `Public a then `Public else
  if List.mem `Protected a then `Protected else
  if List.mem `Private a then `Private else
  `Default

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
| jasmin_header inners fields methods { $1 $2 $3 $4 }

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
    debug_extension {

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
| PUBLIC
  {
    `Public
  }
| PRIVATE
  {
    `Private
  }
| PROTECTED
  {
    `Protected
  }
| STATIC
  {
    `Static
  }
| FINAL
  {
    `Final
  }
| SYNCHRONIZED
  {
    `Synchronized
  }
| VOLATILE
  {
    `Volatile
  }
| TRANSIENT
  {
    `Transient
  }
| NATIVE
  {
    `Native
  }
| INTERFACE
  {
    `Interface
  }
| ABSTRACT
  {
    `Abstract
  }
| ANNOTATION
  {
    `Annotation
  }
| ENUM
  {
    `Enum
  }
| BRIDGE
  {
    `Bridge
  }
| VARARGS
  {
    `Varargs
  }
| STRICT
  {
    `Strict
  }
| SYNTHETIC
  {
    `Synthetic
  }

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
  let code =[|
    JCode.OpGetStatic (
      (JBasics.make_cn "java.lang.System"),
      (JBasics.make_fs "out"
                      (JBasics.TObject
                         (JBasics.TClass
                            (JBasics.make_cn "java.io.PrintStream")))));
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpStore (`Object, 1);
    (JCode.OpConst(`Byte (10)));
    JCode.OpInvalid;
    JCode.OpStore (`Int2Bool, 2);
    (JCode.OpConst(`Byte (10)));
    JCode.OpInvalid;
    JCode.OpLoad (`Int2Bool, 2);
    (JCode.OpSub `Int2Bool);
    JCode.OpInvoke (
     `Static ((JBasics.make_cn "java.lang.String")),
     (JBasics.make_ms "valueOf" [(JBasics.TBasic `Int)]
                      (Some (JBasics.TObject
                               (JBasics.TClass
                                  (JBasics.make_cn "java.lang.String"))))));
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpStore (`Object, 3);
    JCode.OpLoad (`Object, 1);
    JCode.OpLoad (`Object, 3);
    JCode.OpInvoke (
     `Virtual ((JBasics.TClass
                  (JBasics.make_cn "java.io.PrintStream"))),
     (JBasics.make_ms "println"
                      [(JBasics.TObject
                          (JBasics.TClass
                             (JBasics.make_cn "java.lang.String")))]
                      None));
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpIInc (2, -1);
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpInvalid;
    JCode.OpLoad (`Int2Bool, 2);
    JCode.OpIf (`Ne, -20);
    JCode.OpInvalid;
    JCode.OpInvalid;
    (JCode.OpReturn `Void)
  |] in
  let jmethod = {
    JCode.c_max_stack = 3;
    c_max_locals = 4;
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
    let ms = JBasics.make_ms
        "mm"
        [(JBasics.TObject (JBasics.TArray (JBasics.TObject (JBasics.TClass (JBasics.make_cn "java.lang.String")))))]
        None
    in ($2, ms)
  }

endmethod :
| DEND METHOD SEP
  {
    ""
  }

/* ---- Statements in a method */

statements :
| statements statement
  {
    ""
  }
| statement
  {
    ""
  }

statement :
| stmnt SEP
  {
    ""
  }

stmnt :
| instruction
  {
    ""
  }
| directive
  {
    ""
  }
| error
  {
    ""
  }
| label
  {
    ""
  }
| /* empty */
  {
    ""
  }


/* label: */
label :
| Word COLON
  {
    ""
  }
| Int COLON instruction
  {
    ""
  }

/* Directives (.catch, .set, .limit, etc.) */

directive :
| DVAR var_expr
  {
    ""
  }
| DLIMIT limit_expr
  {
    ""
  }
| DLINE line_expr
  {
    ""
  }
| DTHROWS throws_expr
  {
    ""
  }
| DCATCH catch_expr
  {
    ""
  }
| DSET set_expr
  {
    ""
  }
| DSIGNATURE signature_expr
  {
    ""
  }
| DATTRIBUTE generic_expr
  {
    ""
  }
| DDEPRECATED deprecated_expr
  {
    ""
  }
| DANNOTATION ann_met_expr ann_arglist endannotation
  {
    ""
  }
| DANNOTATION ann_def_spec ann_def_val endannotation
  {
    ""
  }
| DSTACK stackmap
  {
    ""
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
    ""
  }
| STACK Int          /* .limit stack */
  {
    ""
  }
| Word Int
  {
    ""
  }

/* .line <num> */
line_expr :
| Int
  {
    ""
  }

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
| simple_instruction
  {
    ""
  }
| complex_instruction
  {
    ""
  }

/* Various patterns of instruction: */
/*      instruction [<pattern>] */
simple_instruction :
| Insn
  {
    ""
  }
| Insn Int Int
  {
    ""
  }
| Insn Int
  {
    ""
  }
| Insn Num
  {
    ""
  }
| Insn Word
  {
    ""
  }
| Insn Word Int
  {
    ""
  }
| Insn Word Word
  {
    ""
  }
| Insn Str
  {
    ""
  }
| Insn Relative
  {
    ""
  }


/* complex (i.e. multiline) instructions */
/*      lookupswitch <lookup> */
/*      tableswitch  <table> */

complex_instruction :
| LOOKUPSWITCH lookup
  {
    ""
  }
| TABLESWITCH table
  {
    ""
  }

/* lookupswitch */
/*     <value> : <label> */
/*     <value> : <label> */
/*     ... */
/*     default : <label> */

lookup :
| lookup_args lookup_list lookup_default
  {
    ""
  }

lookup_args :
| SEP     /* no arguments to lookupswitch */
  {
    ""
  }


lookup_list :
| lookup_list lookup_entry
  {
    ""
  }
| lookup_entry
  {
    ""
  }

lookup_entry :
| Int COLON Word SEP
  {
    ""
  }
| Int COLON Int SEP
  {
    ""
  }

lookup_default :
| DEFAULT COLON Word
  {
    ""
  }
| DEFAULT COLON Int
  {
    ""
  }


/* tableswitch <low> [<high>] */
/*     <label> */
/*     <label> */
/*     ... */
/*     default : <label> */

table :
| table_args table_list table_default
  {
    ""
  }

table_args :
| Int SEP     /* one argument : the <low> parameter */
  {
    ""
  }
| Int Int SEP     /* two arguments : <low> and <high> parameters */
  {
    ""
  }


table_list :
| table_list table_entry
  {
    ""
  }
| table_entry
  {
    ""
  }

table_entry :
| Word SEP
  {
    ""
  }
| Int SEP
  {
    ""
  }

table_default :
| DEFAULT COLON Word
  {
    ""
  }
| DEFAULT COLON Int
  {
    ""
  }
