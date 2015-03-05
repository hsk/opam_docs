.bytecode 49.0
.source B.java

.class  public B
.super java.lang.Object

.field public f I 

.method static public main([Ljava/lang/String;)V
	.limit stack 1
	.limit locals 3
	.line 7
		0: 	iconst_1
		1: 	istore_2
	.line 8
		2: 	iload_2
		3: 	lookupswitch 
			1 : 		AA
			default : 	AA
	.line 9
		AA:
		 	iconst_1
		 	istore_1
	.line 11
		 	iload_2
		 	tableswitch 1
					A48
					A50
					A52
			default : 	A54
	.line 12
		A48:
		 	iconst_1
		A49:
		 	istore_1
	.line 13
		A50:
		 	iconst_1
		A51:
		 	istore_1
	.line 14
		A52:
		 	iconst_1
		A53:
		 	istore_1
	.line 16
		A54:
		 	return
.end method

.method public <init>()V
	.limit stack 1
	.limit locals 1
	.line 1
		0: 	aload_0
		1: 	invokenonvirtual java/lang/Object/<init>()V
		4: 	return
.end method

