open Javalib_pack
open Javalib
let _ =
let k =
(JClass
   { c_name = (JBasics.make_cn "A");
     c_version = { JBasics.major = 52; minor = 0 };
     c_access = `Public;
     c_final = false;
     c_abstract = false;
     c_super_class = (Some (JBasics.make_cn "java.lang.Object"));
     c_generic_signature = None;
     c_fields = begin let map = JBasics.FieldMap.empty in let map =
                JBasics.FieldMap.add
                (JBasics.make_fs "f" (JBasics.TBasic `Int))
                { cf_signature = (JBasics.make_fs "f" (JBasics.TBasic `Int));
                  cf_class_signature = (JBasics.make_cfs (JBasics.make_cn "A")
                                                         (JBasics.make_fs 
                                                         "f"
                                                         (JBasics.TBasic `Int)));
                  cf_generic_signature = None; cf_access = `Public;
                  cf_static = false; cf_synthetic = false; cf_enum = false;
                  cf_kind = NotFinal; cf_value = None; cf_transient = false;
                  cf_annotations = []; cf_other_flags = [];
                  cf_attributes = { synthetic = false; deprecated = false;
                                    other = [] } } map in map
     end;
     c_interfaces = [];
     c_consts = [|
       JBasics.ConstUnusable;
       (JBasics.ConstMethod
          ((JBasics.TClass (JBasics.make_cn "java.lang.Object")),
           (JBasics.make_ms "<init>" [] None)));
       (JBasics.ConstValue
          (JBasics.ConstClass (JBasics.TClass (JBasics.make_cn "A"))));
       (JBasics.ConstValue
          (JBasics.ConstClass
             (JBasics.TClass (JBasics.make_cn "java.lang.Object"))));
       (JBasics.ConstStringUTF8 "f");
       (JBasics.ConstStringUTF8 "I");
       (JBasics.ConstStringUTF8 "<init>");
       (JBasics.ConstStringUTF8 "()V");
       (JBasics.ConstStringUTF8 "Code");
       (JBasics.ConstStringUTF8 "LineNumberTable");
       (JBasics.ConstStringUTF8 "m");
       (JBasics.ConstStringUTF8 "(Ljava/lang/String;)V");
       (JBasics.ConstStringUTF8 "StackMapTable");
       (JBasics.ConstStringUTF8 "SourceFile");
       (JBasics.ConstStringUTF8 "A.java");
       JBasics.ConstNameAndType ("<init>", (JBasics.SMethod ([], None)));
       (JBasics.ConstStringUTF8 "A");
       (JBasics.ConstStringUTF8 "java/lang/Object")
     |];
     c_sourcefile = (Some "A.java");
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
       (JBasics.make_ms "m"
                        [(JBasics.TObject
                            (JBasics.TClass
                               (JBasics.make_cn "java.lang.String")))] None)
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "m"
                               [(JBasics.TObject
                                   (JBasics.TClass
                                      (JBasics.make_cn "java.lang.String")))]
                               None);
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "A")
                                (JBasics.make_ms "m"
                                                 [(JBasics.TObject
                                                     (JBasics.TClass
                                                        (JBasics.make_cn "java.lang.String")))]
                                                 None));
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
                   c_max_locals = 4;
                   c_code =
                     [|(JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 3);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpLookupSwitch (19, [(1l, 17)]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpLookupSwitch (29, [(1l, 25); (2l, 27)]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (33, 1l, 3l, [|27; 29; 31|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (37, 1l, 4l, [|29; 31; 33; 35|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (45, 1l, 5l,
                         [|35; 37; 39; 41; 43|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (49, 1l, 6l,
                         [|37; 39; 41; 43; 45; 47|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (57, 1l, 7l,
                         [|43; 45; 47; 49; 51; 53; 55|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (61, 1l, 8l,
                         [|45; 47; 49; 51; 53; 55; 57; 59|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (69, 1l, 9l,
                         [|51; 53; 55; 57; 59; 61; 63; 65; 67|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (73, 1l, 10l,
                         [|53; 55; 57; 59; 61; 63; 65; 67; 69; 71|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (81, 1l, 11l,
                         [|59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (85, 1l, 12l,
                         [|61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (93, 1l, 13l,
                         [|67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (97, 1l, 14l,
                         [|69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91;
                           93; 95|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (105, 1l, 15l,
                         [|75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97;
                           99; 101; 103|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (109, 1l, 16l,
                         [|77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99;
                           101; 103; 105; 107|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       JCode.OpLoad (`Int2Bool, 3);
                       JCode.OpTableSwitch (117, 1l, 17l,
                         [|83; 85; 87; 89; 91; 93; 95; 97; 99; 101; 103; 105;
                           107; 109; 111; 113; 115|]);
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       JCode.OpInvalid;
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpConst(`Int (1l)));
                       JCode.OpStore (`Int2Bool, 2);
                       (JCode.OpReturn `Void)|];
                   c_exc_tbl = [];
                   c_line_number_table = (Some [(0, 7);
                                                (2, 8);
                                                (20, 9);
                                                (22, 11);
                                                (48, 12);
                                                (50, 13);
                                                (52, 15);
                                                (80, 16);
                                                (82, 17);
                                                (84, 18);
                                                (86, 20);
                                                (116, 21);
                                                (118, 22);
                                                (120, 23);
                                                (122, 24);
                                                (124, 26);
                                                (160, 27);
                                                (162, 28);
                                                (164, 29);
                                                (166, 30);
                                                (168, 31);
                                                (170, 33);
                                                (208, 34);
                                                (210, 35);
                                                (212, 36);
                                                (214, 37);
                                                (216, 38);
                                                (218, 39);
                                                (220, 41);
                                                (264, 42);
                                                (266, 43);
                                                (268, 44);
                                                (270, 45);
                                                (272, 46);
                                                (274, 47);
                                                (276, 48);
                                                (278, 50);
                                                (324, 51);
                                                (326, 52);
                                                (328, 53);
                                                (330, 54);
                                                (332, 55);
                                                (334, 56);
                                                (336, 57);
                                                (338, 58);
                                                (340, 60);
                                                (392, 61);
                                                (394, 62);
                                                (396, 63);
                                                (398, 64);
                                                (400, 65);
                                                (402, 66);
                                                (404, 67);
                                                (406, 68);
                                                (408, 69);
                                                (410, 71);
                                                (464, 72);
                                                (466, 73);
                                                (468, 74);
                                                (470, 75);
                                                (472, 76);
                                                (474, 77);
                                                (476, 78);
                                                (478, 79);
                                                (480, 80);
                                                (482, 81);
                                                (484, 83);
                                                (544, 84);
                                                (546, 85);
                                                (548, 86);
                                                (550, 87);
                                                (552, 88);
                                                (554, 89);
                                                (556, 90);
                                                (558, 91);
                                                (560, 92);
                                                (562, 93);
                                                (564, 94);
                                                (566, 96);
                                                (628, 97);
                                                (630, 98);
                                                (632, 99);
                                                (634, 100);
                                                (636, 101);
                                                (638, 102);
                                                (640, 103);
                                                (642, 104);
                                                (644, 105);
                                                (646, 106);
                                                (648, 107);
                                                (650, 108);
                                                (652, 110);
                                                (720, 111);
                                                (722, 112);
                                                (724, 113);
                                                (726, 114);
                                                (728, 115);
                                                (730, 116);
                                                (732, 117);
                                                (734, 118);
                                                (736, 119);
                                                (738, 120);
                                                (740, 121);
                                                (742, 122);
                                                (744, 123);
                                                (746, 125);
                                                (816, 126);
                                                (818, 127);
                                                (820, 128);
                                                (822, 129);
                                                (824, 130);
                                                (826, 131);
                                                (828, 132);
                                                (830, 133);
                                                (832, 134);
                                                (834, 135);
                                                (836, 136);
                                                (838, 137);
                                                (840, 138);
                                                (842, 139);
                                                (844, 141);
                                                (920, 142);
                                                (922, 143);
                                                (924, 144);
                                                (926, 145);
                                                (928, 146);
                                                (930, 147);
                                                (932, 148);
                                                (934, 149);
                                                (936, 150);
                                                (938, 151);
                                                (940, 152);
                                                (942, 153);
                                                (944, 154);
                                                (946, 155);
                                                (948, 156);
                                                (950, 158);
                                                (1028, 159);
                                                (1030, 160);
                                                (1032, 161);
                                                (1034, 162);
                                                (1036, 163);
                                                (1038, 164);
                                                (1040, 165);
                                                (1042, 166);
                                                (1044, 167);
                                                (1046, 168);
                                                (1048, 169);
                                                (1050, 170);
                                                (1052, 171);
                                                (1054, 172);
                                                (1056, 173);
                                                (1058, 174);
                                                (1060, 176);
                                                (1144, 177);
                                                (1146, 178);
                                                (1148, 179);
                                                (1150, 180);
                                                (1152, 181);
                                                (1154, 182);
                                                (1156, 183);
                                                (1158, 184);
                                                (1160, 185);
                                                (1162, 186);
                                                (1164, 187);
                                                (1166, 188);
                                                (1168, 189);
                                                (1170, 190);
                                                (1172, 191);
                                                (1174, 192);
                                                (1176, 193);
                                                (1178, 195)]);
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = (Some [(20,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (22,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (48,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (50,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (52,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (80,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (82,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (84,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (86,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (116,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (118,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (120,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (122,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (124,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (160,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (162,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (164,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (166,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (168,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (170,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (208,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (210,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (212,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (214,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (216,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (218,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (220,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (264,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (266,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (268,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (270,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (272,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (274,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (276,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (278,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (324,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (326,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (328,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (330,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (332,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (334,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (336,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (338,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (340,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (392,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (394,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (396,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (398,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (400,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (402,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (404,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (406,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (408,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (410,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (464,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (466,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (468,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (470,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (472,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (474,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (476,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (478,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (480,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (482,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (484,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (544,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (546,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (548,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (550,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (552,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (554,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (556,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (558,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (560,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (562,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (564,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (566,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (628,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (630,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (632,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (634,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (636,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (638,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (640,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (642,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (644,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (646,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (648,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (650,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (652,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (720,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (722,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (724,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (726,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (728,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (730,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (732,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (734,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (736,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (738,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (740,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (742,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (744,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (746,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (816,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (818,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (820,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (822,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (824,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (826,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (828,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (830,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (832,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (834,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (836,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (838,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (840,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (842,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (844,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (920,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (922,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (924,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (926,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (928,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (930,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (932,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (934,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (936,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (938,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (940,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (942,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (944,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (946,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (948,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (950,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1028,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1030,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1032,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1034,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1036,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1038,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1040,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1042,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1044,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1046,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1048,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1050,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1052,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1054,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1056,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1058,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1060,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1144,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1146,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1148,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1150,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1152,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1154,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1156,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1158,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1160,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1162,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1164,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1166,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1168,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1170,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1172,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1174,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1176,
                                               [JBasics.VTop;
                                                JBasics.VInteger], []);
                                              (1178,
                                               [JBasics.VTop;
                                                JBasics.VInteger], [])]);
                   c_attributes = [] })))
          }) methods in
       let methods = JBasics.MethodMap.add (JBasics.make_ms "<init>" [] None)
       (ConcreteMethod
          { cm_signature =
              (JBasics.make_ms "<init>" [] None);
            cm_class_method_signature =
              (JBasics.make_cms (JBasics.make_cn "A")
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
                       JCode.OpInvalid;
                       (JCode.OpReturn `Void)|];
                   c_exc_tbl = [];
                   c_line_number_table = (Some [(0, 1)]);
                   c_local_variable_table = None;
                   c_local_variable_type_table = None;
                   c_stack_map_midp = None;
                   c_stack_map_java6 = None;
                   c_attributes = [] })))
          }) methods in
       methods
     end;
      })
  in Javalib.unparse_class k (open_out "A.class");
  JPrint.print_jasmin k stdout;

