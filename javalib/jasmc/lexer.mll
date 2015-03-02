{
  open Parser
       
  let get = Lexing.lexeme

  let keyword_table = Hashtbl.create 53
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      [
        (* reserved_words used in Jasmin directives *)
        "field", FIELD;
        "from", FROM;
        "method", METHOD;
        "to", TO;
        "is", IS;
        "using", USING;
        "signature", SIGNATURE;
        "stack", STACK;
        "offset", OFFSET;
        "locals", LOCALS;
        "use", USE;
        "inner", INNER;
        "outer", OUTER;
        "class", CLASS;
        "visible", VISIBLE;
        "invisible", INVISIBLE;
        "visibleparam", VISIBLEPARAM;
        "invisibleparam", INVISIBLEPARAM;

        (* Special-case instructions *)
        "tableswitch", TABLESWITCH;
        "lookupswitch", LOOKUPSWITCH;
        "default", DEFAULT;

        (* Access flags *)
        "public", PUBLIC;
        "private", PRIVATE;
        "protected", PROTECTED;
        "static", STATIC;
        "final", FINAL;
        "synchronized", SYNCHRONIZED;
        "volatile", VOLATILE;
        "transient", TRANSIENT;
        "native", NATIVE;
        "interface", INTERFACE;
        "abstract", ABSTRACT;

        "annotation", ANNOTATION;
        "enum", ENUM;
        "bridge", BRIDGE;
        "varargs", VARARGS;
        "fpstrict", STRICT;
        "synthetic", SYNTHETIC;
 ]
  let dbg a = ()
}
(*******************************************************************
 * Helpers                                                         *
 *******************************************************************)

let latin1_input_character = ['\000'- '\255']
let ht = '\t'
let lf = '\n'
let ff = '\012'
let cr = '\r'
let sp = ' '

let line_terminator = lf | cr | cr lf 
let input_character = ['\000'-'\255'] # ['\r' '\n'] (* # (lf | cr) *)

let not_star_not_newline = (['\000'-'\255'] # ['\r''\n''*'])
let not_star_not_slash_not_newline = (['\000'-'\255'] # ['\r''\n''*''/'])

let digit = ['0'-'9']
let non_zero_digit = ['1'-'9']

let decimal_numeral = '0' | non_zero_digit digit*

let latin1_letter =
       ['A'-'Z'] | ['a'-'z'] | ['\170'-'\170'] | ['\181'-'\181'] |
       ['\186'-'\186'] | ['\192'-'\214'] | ['\216'-'\246'] | ['\248'-'\255']

let java_letter = latin1_letter | '$' | '_'
let java_letter_or_digit = latin1_letter | digit | '$' | '_'

let string_character = (['\000'-'\255'] # ['\r' '\n' '"' '\\'])
(*******************************************************************
 * Tokens                                                          *
 *******************************************************************)

rule token = parse
  | eof    { EOF }
  
  (* Whitespace *)
  | (sp | ht | ff)                  { token lexbuf }
  | line_terminator+                 { dbg("SEP");SEP }
  | ";" input_character*
                                    { dbg(Lexing.lexeme lexbuf); token lexbuf } (* end_of_line_comment *)
  | ";" input_character* line_terminator+
                                    { dbg(Lexing.lexeme lexbuf); Lexing.new_line lexbuf; token lexbuf } (* end_of_line_comment *)


  | "aaload" { Insn("aaload", "") }
  | "aastore" { Insn("aastore", "") }
  | "aconst_null" { Insn("aconst_null", "") }
  | "aload" { Insn("aload", "i") }
  | "aload_w" { Insn("aload", "I") }
  | "aload_0" { Insn("aload_0", "") }
  | "aload_1" { Insn("aload_1", "") }
  | "aload_2" { Insn("aload_2", "") }
  | "aload_3" { Insn("aload_3", "") }
  | "anewarray" { Insn("anewarray", "class") }
  | "areturn" { Insn("areturn", "") }
  | "arraylength" { Insn("arraylength", "") }
  | "astore" { Insn("astore", "i") }
  | "astore_w" { Insn("astore", "I") }
  | "astore_0" { Insn("astore_0", "") }
  | "astore_1" { Insn("astore_1", "") }
  | "astore_2" { Insn("astore_2", "") }
  | "astore_3" { Insn("astore_3", "") }
  | "athrow" { Insn("athrow", "") }
  | "baload" { Insn("baload", "") }
  | "bastore" { Insn("bastore", "") }
  | "bipush" { Insn("bipush", "i") }
  | "breakpoint" { Insn("breakpoint", "") }
  | "caload" { Insn("caload", "") }
  | "castore" { Insn("castore", "") }
  | "checkcast" { Insn("checkcast", "class") }
  | "d2f" { Insn("d2f", "") }
  | "d2i" { Insn("d2i", "") }
  | "d2l" { Insn("d2l", "") }
  | "dadd" { Insn("dadd", "") }
  | "daload" { Insn("daload", "") }
  | "dastore" { Insn("dastore", "") }
  | "dcmpg" { Insn("dcmpg", "") }
  | "dcmpl" { Insn("dcmpl", "") }
  | "dconst_0" { Insn("dconst_0", "") }
  | "dconst_1" { Insn("dconst_1", "") }
  | "ddiv" { Insn("ddiv", "") }
  | "dload" { Insn("dload", "i") }
  | "dload_w" { Insn("dload", "I") }
  | "dload_0" { Insn("dload_0", "") }
  | "dload_1" { Insn("dload_1", "") }
  | "dload_2" { Insn("dload_2", "") }
  | "dload_3" { Insn("dload_3", "") }
  | "dmul" { Insn("dmul", "") }
  | "dneg" { Insn("dneg", "") }
  | "drem" { Insn("drem", "") }
  | "dreturn" { Insn("dreturn", "") }
  | "dstore" { Insn("dstore", "i") }
  | "dstore_w" { Insn("dstore", "I") }
  | "dstore_0" { Insn("dstore_0", "") }
  | "dstore_1" { Insn("dstore_1", "") }
  | "dstore_2" { Insn("dstore_2", "") }
  | "dstore_3" { Insn("dstore_3", "") }
  | "dsub" { Insn("dsub", "") }
  | "dup" { Insn("dup", "") }
  | "dup2" { Insn("dup2", "") }
  | "dup2_x1" { Insn("dup2_x1", "") }
  | "dup2_x2" { Insn("dup2_x2", "") }
  | "dup_x1" { Insn("dup_x1", "") }
  | "dup_x2" { Insn("dup_x2", "") }
  | "f2d" { Insn("f2d", "") }
  | "f2i" { Insn("f2i", "") }
  | "f2l" { Insn("f2l", "") }
  | "fadd" { Insn("fadd", "") }
  | "faload" { Insn("faload", "") }
  | "fastore" { Insn("fastore", "") }
  | "fcmpg" { Insn("fcmpg", "") }
  | "fcmpl" { Insn("fcmpl", "") }
  | "fconst_0" { Insn("fconst_0", "") }
  | "fconst_1" { Insn("fconst_1", "") }
  | "fconst_2" { Insn("fconst_2", "") }
  | "fdiv" { Insn("fdiv", "") }
  | "fload" { Insn("fload", "i") }
  | "fload_w" { Insn("fload", "I") }
  | "fload_0" { Insn("fload_0", "") }
  | "fload_1" { Insn("fload_1", "") }
  | "fload_2" { Insn("fload_2", "") }
  | "fload_3" { Insn("fload_3", "") }
  | "fmul" { Insn("fmul", "") }
  | "fneg" { Insn("fneg", "") }
  | "frem" { Insn("frem", "") }
  | "freturn" { Insn("freturn", "") }
  | "fstore" { Insn("fstore", "i") }
  | "fstore_w" { Insn("fstore", "I") }
  | "fstore_0" { Insn("fstore_0", "") }
  | "fstore_1" { Insn("fstore_1", "") }
  | "fstore_2" { Insn("fstore_2", "") }
  | "fstore_3" { Insn("fstore_3", "") }
  | "fsub" { Insn("fsub", "") }
  | "getfield" { Insn("getfield", "field") }
  | "getstatic" { Insn("getstatic", "field") }
  | "goto" { Insn("goto", "label") }
  | "goto_w" { Insn("goto_w", "label") }
  | "i2d" { Insn("i2d", "") }
  | "i2f" { Insn("i2f", "") }
  | "i2l" { Insn("i2l", "") }
  | "iadd" { Insn("iadd", "") }
  | "iaload" { Insn("iaload", "") }
  | "iand" { Insn("iand", "") }
  | "iastore" { Insn("iastore", "") }
  | "iconst_0" { Insn("iconst_0", "") }
  | "iconst_1" { Insn("iconst_1", "") }
  | "iconst_2" { Insn("iconst_2", "") }
  | "iconst_3" { Insn("iconst_3", "") }
  | "iconst_4" { Insn("iconst_4", "") }
  | "iconst_5" { Insn("iconst_5", "") }
  | "iconst_m1" { Insn("iconst_m1", "") }
  | "idiv" { Insn("idiv", "") }
  | "if_acmpeq" { Insn("if_acmpeq", "label") }
  | "if_acmpne" { Insn("if_acmpne", "label") }
  | "if_icmpeq" { Insn("if_icmpeq", "label") }
  | "if_icmpge" { Insn("if_icmpge", "label") }
  | "if_icmpgt" { Insn("if_icmpgt", "label") }
  | "if_icmple" { Insn("if_icmple", "label") }
  | "if_icmplt" { Insn("if_icmplt", "label") }
  | "if_icmpne" { Insn("if_icmpne", "label") }
  | "ifeq" { Insn("ifeq", "label") }
  | "ifge" { Insn("ifge", "label") }
  | "ifgt" { Insn("ifgt", "label") }
  | "ifle" { Insn("ifle", "label") }
  | "iflt" { Insn("iflt", "label") }
  | "ifne" { Insn("ifne", "label") }
  | "ifnonnull" { Insn("ifnonnull", "label") }
  | "ifnull" { Insn("ifnull", "label") }
  | "iinc" { Insn("iinc", "ii") }
  | "iinc_w" { Insn("iinc", "Ii") }
  | "iload" { Insn("iload", "i") }
  | "iload_w" { Insn("iload", "I") }
  | "iload_0" { Insn("iload_0", "") }
  | "iload_1" { Insn("iload_1", "") }
  | "iload_2" { Insn("iload_2", "") }
  | "iload_3" { Insn("iload_3", "") }
  | "imul" { Insn("imul", "") }
  | "ineg" { Insn("ineg", "") }
  | "instanceof" { Insn("instanceof", "class") }
  | "int2byte" { Insn("int2byte", "") }
  | "int2char" { Insn("int2char", "") }
  | "int2short" { Insn("int2short", "") }
  
  (* added this synonym *)
  | "i2b" { Insn("int2byte", "") }
  
  (* added this synonym *)
  | "i2c" { Insn("int2char", "") }
  
  (* added this synonym *)
  | "i2s" { Insn("int2short", "") }
  | "invokedynamic" { Insn("invokedynamic", "method") }
  | "invokeinterface" { Insn("invokeinterface", "interface") }
  | "invokenonvirtual" { Insn("invokenonvirtual", "method") }
  
  (* added this synonym *)
  | "invokespecial" { Insn("invokenonvirtual", "method") }
  | "invokestatic" { Insn("invokestatic", "method") }
  | "invokevirtual" { Insn("invokevirtual", "method") }
  | "ior" { Insn("ior", "") }
  | "irem" { Insn("irem", "") }
  | "ireturn" { Insn("ireturn", "") }
  | "ishl" { Insn("ishl", "") }
  | "ishr" { Insn("ishr", "") }
  | "istore" { Insn("istore", "i") }
  | "istore_w" { Insn("istore", "I") }
  | "istore_0" { Insn("istore_0", "") }
  | "istore_1" { Insn("istore_1", "") }
  | "istore_2" { Insn("istore_2", "") }
  | "istore_3" { Insn("istore_3", "") }
  | "isub" { Insn("isub", "") }
  | "iushr" { Insn("iushr", "") }
  | "ixor" { Insn("ixor", "") }
  | "jsr" { Insn("jsr", "label") }
  | "jsr_w" { Insn("jsr_w", "label") }
  | "l2d" { Insn("l2d", "") }
  | "l2f" { Insn("l2f", "") }
  | "l2i" { Insn("l2i", "") }
  | "ladd" { Insn("ladd", "") }
  | "laload" { Insn("laload", "") }
  | "land" { Insn("land", "") }
  | "lastore" { Insn("lastore", "") }
  | "lcmp" { Insn("lcmp", "") }
  | "lconst_0" { Insn("lconst_0", "") }
  | "lconst_1" { Insn("lconst_1", "") }
  | "ldc" { Insn("ldc", "constant") }
  | "ldc_w" { Insn("ldc_w", "constant") }
  | "ldc2_w" { Insn("ldc2_w", "bigconstant") }
  | "ldiv" { Insn("ldiv", "") }
  | "lload" { Insn("lload", "i") }
  | "lload_w" { Insn("lload", "I") }
  | "lload_0" { Insn("lload_0", "") }
  | "lload_1" { Insn("lload_1", "") }
  | "lload_2" { Insn("lload_2", "") }
  | "lload_3" { Insn("lload_3", "") }
  | "lmul" { Insn("lmul", "") }
  | "lneg" { Insn("lneg", "") }
  | "lookupswitch" { Insn("lookupswitch", "switch") }
  | "lor" { Insn("lor", "") }
  | "lrem" { Insn("lrem", "") }
  | "lreturn" { Insn("lreturn", "") }
  | "lshl" { Insn("lshl", "") }
  | "lshr" { Insn("lshr", "") }
  | "lstore" { Insn("lstore", "i") }
  | "lstore_w" { Insn("lstore", "I") }
  | "lstore_0" { Insn("lstore_0", "") }
  | "lstore_1" { Insn("lstore_1", "") }
  | "lstore_2" { Insn("lstore_2", "") }
  | "lstore_3" { Insn("lstore_3", "") }
  | "lsub" { Insn("lsub", "") }
  | "lushr" { Insn("lushr", "") }
  | "lxor" { Insn("lxor", "") }
  | "monitorenter" { Insn("monitorenter", "") }
  | "monitorexit" { Insn("monitorexit", "") }
  | "multianewarray" { Insn("multianewarray", "marray") }
  | "new" { Insn("new", "class") }
  | "newarray" { Insn("newarray", "atype") }
  | "nop" { Insn("nop", "") }
  | "pop" { Insn("pop", "") }
  | "pop2" { Insn("pop2", "") }
  | "putfield" { Insn("putfield", "field") }
  | "putstatic" { Insn("putstatic", "field") }
  | "ret" { Insn("ret", "i") }
  | "ret_w" { Insn("ret", "I") }
  | "return" { Insn("return", "") }
  | "saload" { Insn("saload", "") }
  | "sastore" { Insn("sastore", "") }
  | "sipush" { Insn("sipush", "i") }
  | "swap" { Insn("swap", "") }
  | "tableswitch" { Insn("tableswitch", "switch") }

  (* Jasmin directives *)
  | ".annotation" { dbg(Lexing.lexeme lexbuf); DANNOTATION }
  | ".attribute" { dbg(Lexing.lexeme lexbuf); DATTRIBUTE }
  | ".bytecode" { dbg(Lexing.lexeme lexbuf); DBYTECODE }
  | ".catch" { dbg(Lexing.lexeme lexbuf); DCATCH }
  | ".class" { dbg(Lexing.lexeme lexbuf); DCLASS }
  | ".deprecated" { dbg(Lexing.lexeme lexbuf); DDEPRECATED }
  | ".end" { dbg(Lexing.lexeme lexbuf); DEND }
  | ".field" { dbg(Lexing.lexeme lexbuf); DFIELD }
  | ".implements" { dbg(Lexing.lexeme lexbuf); DIMPLEMENTS }
  | ".inner" { dbg(Lexing.lexeme lexbuf); DINNER }
  | ".interface" { dbg(Lexing.lexeme lexbuf); DINTERFACE }
  | ".limit" { dbg(Lexing.lexeme lexbuf); DLIMIT }
  | ".line" { dbg(Lexing.lexeme lexbuf); DLINE }
  | ".method" { dbg(Lexing.lexeme lexbuf); DMETHOD }
  | ".set" { dbg(Lexing.lexeme lexbuf); DSET }
  | ".source" { dbg(Lexing.lexeme lexbuf); DSOURCE }
  | ".super" { dbg(Lexing.lexeme lexbuf); DSUPER }
  | ".throws" { dbg(Lexing.lexeme lexbuf); DTHROWS }
  | ".var" { dbg(Lexing.lexeme lexbuf); DVAR }
  | ".debug" { dbg(Lexing.lexeme lexbuf); DDEBUG }
  | ".enclosing" { dbg(Lexing.lexeme lexbuf); DENCLOSING }
  | ".signature" { dbg(Lexing.lexeme lexbuf); DSIGNATURE }
  | ".stack" { dbg(Lexing.lexeme lexbuf); DSTACK }
  | decimal_numeral '.' decimal_numeral as i { dbg(Lexing.lexeme lexbuf); Num (i) }
  | decimal_numeral as i                    { dbg(Lexing.lexeme lexbuf); Int (int_of_string i) }
  | '"' string_character* '"' as s          { dbg(Lexing.lexeme lexbuf); Str s }
  | ':' { dbg(Lexing.lexeme lexbuf); COLON }
  | '=' { dbg(Lexing.lexeme lexbuf); EQ }
  | [ ^ ' ' '\r' '\n' '\t' '=' ':' ]+ as id { dbg(Lexing.lexeme lexbuf); try
            Hashtbl.find keyword_table id
                                              with Not_found ->
            Word id }
