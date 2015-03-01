open Javalib_pack

let make_fields class_name =
  let fields = JBasics.FieldMap.empty in
  let field_signature = JBasics.make_fs "f" (JBasics.TBasic `Int) in
  let class_field_signature = JBasics.make_cfs class_name field_signature in
  let class_field = {
    Javalib.cf_signature= field_signature;
    cf_class_signature = class_field_signature;
    cf_generic_signature = None;
    cf_access = `Default;
    cf_static = false;
    cf_synthetic = false;
    cf_enum = false;
    cf_kind = Javalib.NotFinal;
    cf_value = None;
    cf_transient = false;
    cf_annotations = [];
    cf_other_flags = [];
    cf_attributes = { 
      Javalib.synthetic = false;
      deprecated = false;
      other = [];
    };
  } in
  let fields = JBasics.FieldMap.add field_signature class_field fields in
  fields

let make_methods class_name super_class_name =
  let methods = JBasics.MethodMap.empty in
  let java_lang_string = JBasics.make_cn "java.lang.String" in
  let method_signature = JBasics.make_ms "m" [JBasics.TObject (JBasics.TClass java_lang_string)] None in
  let class_method_signature = JBasics.make_cms class_name method_signature in
  let jmethod = {
    JCode.c_max_stack = 0;
    c_max_locals = 2;
    c_code = [|JCode.OpReturn `Void|];
    c_exc_tbl = [];
    c_line_number_table = None;
    c_local_variable_table = None;
    c_local_variable_type_table = None;
    c_stack_map_midp = None;
    c_stack_map_java6 = None;
    c_attributes = [];
  } in
  let rec concrete_method = Javalib.ConcreteMethod {
    Javalib.cm_signature = method_signature;
    cm_class_method_signature = class_method_signature;
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
    cm_attributes = {
      Javalib.synthetic = false;
      deprecated = false;
      other = []
    };
    cm_annotations = {
      Javalib.ma_global = [];
      ma_parameters = []
    }; 
    cm_implementation = Javalib.Java (lazy jmethod)
  } in

  let methods = JBasics.MethodMap.add method_signature concrete_method methods in
  methods

let make_jclass class_name super_class_name fields methods =
  Javalib.JClass {
    Javalib.c_name = class_name;
    c_version = {
      JBasics.major = 52;
      minor = 0
    }; 
    c_access = `Public;
    c_final = false;
    c_abstract = false;
    c_super_class = Some super_class_name; 
    c_generic_signature = None;
    c_fields = fields;
    c_interfaces = [];
    c_consts = [||];
    c_sourcefile = Some "B.java";
    c_deprecated = false;
    c_enclosing_method = None; 
    c_source_debug_extention = None;
    c_inner_classes = [];
    c_synthetic = false; 
    c_enum = false;
    c_annotations = [];
    c_other_flags = [];
    c_other_attributes = []; 
    c_methods = methods
  }

let _ =

  let class_name = JBasics.make_cn "B" in
  let super_class_name = JBasics.make_cn "Object" in
  let fields = make_fields class_name in
  let methods = make_methods class_name super_class_name in

  let jclass = make_jclass class_name super_class_name fields methods in

  Javalib.JPrint.print_jasmin jclass stdout;
  Javalib.unparse_class jclass (open_out "B.class");

