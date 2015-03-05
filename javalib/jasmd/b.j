.source                  B.java
.class                   public B
.super                   java/lang/Object

.field                   public f I

.method                  public <init>()V
   .limit stack          1
   .limit locals         1
   .line                 1
   aload_0               
   invokespecial         java/lang/Object/<init>()V
   return                
.end method              

.method                  public static main([Ljava/lang/String;)V
   .limit stack          1
   .limit locals         3
   .line                 7
   iconst_1              
   istore_2              
   .line                 8
   iload_2               
   lookupswitch          
        1 :              LABEL0x14
        default :        LABEL0x16

   .line                 9
LABEL0x14:
   iconst_1              
   istore_1              
   .line                 11
   iload_2               
   tableswitch           1 3
                         LABEL0x30
                         LABEL0x32
                         LABEL0x34
       default :         LABEL0x36

   .line                 12
LABEL0x30:
   iconst_1              
   istore_1              
   .line                 13
LABEL0x32:
   iconst_1              
   istore_1              
   .line                 14
LABEL0x34:
   iconst_1              
   istore_1              
   .line                 16
   return                
.end method              

