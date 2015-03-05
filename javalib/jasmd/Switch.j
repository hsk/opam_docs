.class public Switch
.super java/lang/Object

.method public <init>()V
   aload_0
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method public static main([Ljava/lang/String;)V
   .limit stack 3

   iconst_1
   lookupswitch
	1 : Hello
	2 : Goodbye
     default : Foo

   iconst_1
   tableswitch 0
	Hello
	Goodbye
     default : Foo

Hello:
Goodbye:
Foo:

   return

.end method

