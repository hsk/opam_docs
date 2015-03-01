open Javalib_pack
open Javalib
let _ =
let k =
(JClass
   { c_name = (JBasics.make_cn "calc");
     c_version = { JBasics.major = 52; minor = 0 };
     c_access = `Public;
     c_final = false;
     c_abstract = false;
     c_super_class = (Some (JBasics.make_cn "java.lang.Object"));
     c_generic_signature = None;
     c_fields = begin let map = JBasics.FieldMap.empty in map
     end;
     c_interfaces = [];
     c_consts = [||];
     c_sourcefile = (Some "calc.java");
     c_deprecated = false;
     c_enclosing_method = None;
     c_source_debug_extention = None;
     c_inner_classes = [];
     c_synthetic = false;
     c_enum = false;
     c_annotations = [];
     c_other_flags = [];
     c_other_attributes = [];
     c_methods = begin
       let methods = JBasics.MethodMap.empty in
       let methods = JBasics.MethodMap.add
       (JBasics.make_ms "main"
                        [(JBasics.TObject
                            (JBasics.TArray
                               (JBasics.TObject
                                  (JBasics.TClass
                                     (JBasics.make_cn "java.lang.String")))))]
                        None)
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "main"
                               [(JBasics.TObject
                                   (JBasics.TArray
                                      (JBasics.TObject
                                         (JBasics.TClass
                                            (JBasics.make_cn "java.lang.String")))))]
                               None);
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "main"
                                                 [(JBasics.TObject
                                                     (JBasics.TArray
                                                        (JBasics.TObject
                                                           (JBasics.TClass
                                                              (JBasics.make_cn "java.lang.String")))))]
                                                 None));
            cm_static = true;
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 2;
                   c_max_locals = 1;
                   c_code =
                     [|(JCode.OpNew (JBasics.make_cn "calc"));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpDup;
                       JCode.OpInvoke (`Special ((JBasics.make_cn "calc")),
                         (JBasics.make_ms "<init>" [] None));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvoke (
                         `Virtual ((JBasics.TClass (JBasics.make_cn "calc"))),
                         (JBasics.make_ms "test" [] None));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpReturn `Void)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add
       (JBasics.make_ms "sub" [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                        (Some (JBasics.TBasic `Int)))
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "sub"
                               [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                               (Some (JBasics.TBasic `Int)));
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "sub"
                                                 [(JBasics.TBasic `Int);
                                                  (JBasics.TBasic `Int)]
                                                 (Some (JBasics.TBasic `Int))));
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 2;
                   c_max_locals = 3;
                   c_code =
                     [|JCode.OpLoad (`Int2Bool, 1);
                       JCode.OpLoad (`Int2Bool, 2);
                       (JCode.OpSub `Int2Bool);
                       (JCode.OpReturn `Int2Bool)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add (JBasics.make_ms "test" [] None)
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "test" [] None);
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "test" [] None));
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 7;
                   c_max_locals = 1;
                   c_code =
                     [|JCode.OpGetStatic (
                         (JBasics.make_cn "java.lang.System"),
                         (JBasics.make_fs "out"
                                          (JBasics.TObject
                                             (JBasics.TClass
                                                (JBasics.make_cn "java.io.PrintStream")))));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpLoad (`Object, 0);
                       JCode.OpLoad (`Object, 0);
                       JCode.OpLoad (`Object, 0);
                       (JCode.OpConst(`Byte (10)));
                       JCode.OpInvalid;
                       (JCode.OpConst(`Byte (20)));
                       JCode.OpInvalid;
                       JCode.OpInvoke (
                         `Virtual ((JBasics.TClass (JBasics.make_cn "calc"))),
                         (JBasics.make_ms "add"
                                          [(JBasics.TBasic `Int);
                                           (JBasics.TBasic `Int)]
                                          (Some (JBasics.TBasic `Int))));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpLoad (`Object, 0);
                       (JCode.OpConst(`Int (3l)));
                       (JCode.OpConst(`Int (4l)));
                       JCode.OpInvoke (
                         `Virtual ((JBasics.TClass (JBasics.make_cn "calc"))),
                         (JBasics.make_ms "sub"
                                          [(JBasics.TBasic `Int);
                                           (JBasics.TBasic `Int)]
                                          (Some (JBasics.TBasic `Int))));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvoke (
                         `Virtual ((JBasics.TClass (JBasics.make_cn "calc"))),
                         (JBasics.make_ms "mul"
                                          [(JBasics.TBasic `Int);
                                           (JBasics.TBasic `Int)]
                                          (Some (JBasics.TBasic `Int))));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (2l)));
                       JCode.OpInvoke (
                         `Virtual ((JBasics.TClass (JBasics.make_cn "calc"))),
                         (JBasics.make_ms "div"
                                          [(JBasics.TBasic `Int);
                                           (JBasics.TBasic `Int)]
                                          (Some (JBasics.TBasic `Int))));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvoke (
                         `Virtual ((JBasics.TClass
                                      (JBasics.make_cn "java.io.PrintStream"))),
                         (JBasics.make_ms "println" [(JBasics.TBasic `Int)]
                                          None));
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpReturn `Void)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add
       (JBasics.make_ms "div" [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                        (Some (JBasics.TBasic `Int)))
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "div"
                               [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                               (Some (JBasics.TBasic `Int)));
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "div"
                                                 [(JBasics.TBasic `Int);
                                                  (JBasics.TBasic `Int)]
                                                 (Some (JBasics.TBasic `Int))));
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 2;
                   c_max_locals = 3;
                   c_code =
                     [|JCode.OpLoad (`Int2Bool, 1);
                       JCode.OpLoad (`Int2Bool, 2);
                       (JCode.OpDiv `Int2Bool);
                       (JCode.OpReturn `Int2Bool)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add (JBasics.make_ms "<init>" [] None)
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "<init>" [] None);
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "<init>" [] None));
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 1;
                   c_max_locals = 1;
                   c_code =
                     [|JCode.OpLoad (`Object, 0);
                       JCode.OpInvoke (
                         `Special ((JBasics.make_cn "java.lang.Object")),
                         (JBasics.make_ms "<init>" [] None));
                       JCode.OpInvalid;
                       (JCode.OpReturn `Void)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add
       (JBasics.make_ms "mul" [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                        (Some (JBasics.TBasic `Int)))
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "mul"
                               [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                               (Some (JBasics.TBasic `Int)));
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "mul"
                                                 [(JBasics.TBasic `Int);
                                                  (JBasics.TBasic `Int)]
                                                 (Some (JBasics.TBasic `Int))));
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 2;
                   c_max_locals = 3;
                   c_code =
                     [|JCode.OpLoad (`Int2Bool, 1);
                       JCode.OpLoad (`Int2Bool, 2);
                       (JCode.OpMult `Int2Bool);
                       (JCode.OpReturn `Int2Bool)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add
       (JBasics.make_ms "add" [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                        (Some (JBasics.TBasic `Int)))
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "add"
                               [(JBasics.TBasic `Int); (JBasics.TBasic `Int)]
                               (Some (JBasics.TBasic `Int)));
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "calc")
                                (JBasics.make_ms "add"
                                                 [(JBasics.TBasic `Int);
                                                  (JBasics.TBasic `Int)]
                                                 (Some (JBasics.TBasic `Int))));
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
            cm_attributes = { synthetic = false; deprecated = false;
                              other = [] };
            cm_annotations = { ma_global = []; ma_parameters = [] };
            cm_implementation =
              (Java (lazy(
                 { JCode.c_max_stack = 2;
                   c_max_locals = 3;
                   c_code =
                     [|JCode.OpLoad (`Int2Bool, 1);
                       JCode.OpLoad (`Int2Bool, 2);
                       (JCode.OpAdd `Int2Bool);
                       (JCode.OpReturn `Int2Bool)|];
                   c_exc_tbl = [];
                   c_line_number_table = None;
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       methods
     end;
      })
  in Javalib.unparse_class k (open_out "calc.class");
  JPrint.print_jasmin k stdout;

