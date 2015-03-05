.class public a
.super java/lang/Object 
.method public ()V 
    aload_0 
    invokespecial java/lang/Object/()V 
    return 
.end method 
  
; int read() 
.method public static read()I
    .limit locals 20 
    .limit stack 20 
    ldc 0 
    istore 1  ; this will hold our final integer 
Label1: 
    getstatic java/lang/System/in Ljava/io/InputStream; 
    invokevirtual java/io/InputStream/read()I 
    istore 2 
    iload 2 
    ldc 10   ; the newline delimiter 
    isub 
    ifeq Label2 
    iload 2 
    ldc 32   ; the space delimiter 
    isub 
    ifeq Label2 
    iload 2 
    ldc 48   ; we have our digit in ASCII, have to subtract it from 48 
    isub 
    ldc 10 
    iload 1 
    imul 
    iadd 
    istore 1 
    goto Label1 
Label2: 
    ;when we come here we have our integer computed in Local Variable 1 
    iload 1 
    ireturn 
.end method 

; void print(int) 
.method public static print(I)V 
    .limit locals 5 
    .limit stack 5 
    iload 0 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    swap 
    invokevirtual java/io/PrintStream/print(I)V 
    return 
.end method

; void print(float) 
.method public static print(F)V 
    .limit locals 5 
    .limit stack 5 
    fload 0 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    swap 
    invokevirtual java/io/PrintStream/print(F)V 
    return 
.end method

; void print(string)
.method public static print(Ljava/lang/String;)V
    .limit locals 5 
    .limit stack 5 
    aload 0
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    swap 
    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V 
    return 
.end method

.method public static main([Ljava/lang/String;)V 
    .limit stack  150 
    .limit locals 150 
    ldc 0 
    istore 0  ; initialize x 
    ldc 0 
    istore 1  ; initialize y 
    goto Lmain 
 Lgcd: 
    istore 10   ; store return address in local variable 10 
    istore 12   ; store y in local variable 12 (v in this scope) 
    istore 11   ; store x in local variable 11 (u in this scope) 
    ldc 0 
    istore 13   ; initialize tmp 
    iload 11 
    iload 12 
    isub      ; u-v 
    iflt label1 
    goto label2 
 label1: 
    iload 11 
    istore 13 ; tmp=u 
    iload 12 
    istore 11 ; u=v 
    iload 13 
    istore 12 ; v=tmp 
 label2: 
    ldc 0 
    iload 12 
    isub 
    ifeq label3 
    goto label4 
 label3: 
    iload 11 
    ret 10       ; return from gcd call 
 label4: 
    iload 10 
    iload 11 
    iload 12 
    iload 13  ; push all locals on stack before the recursive call 
    iload 12  ; push argument v 
    iload 11 
    iload 12 
    isub      ; push argument u-v 
    jsr Lgcd  ; the recursive call 
    swap      ; this is for restoring the locals we pushed before jsr 
    istore 13 ; the stack is (....locals,returned_result) 
    swap      ; this "swap" solution works since our recursive 
    istore 12 ; function has one return value 
    swap      ; as all other F^3 functions. 
    istore 11 
    swap 
    istore 10 
              ; now we have restored our locals 
              ; the stack is (....returned_result) 
    ret 10    ; we return the returned_result of the recursive call 
  
Lmain: 
    
    ldc "Enter two numbers: "
    invokestatic gcd.print(Ljava/lang/String;)V
    invokestatic gcd.read()I 
    istore 0                 ; read x;
    invokestatic gcd.read()I 
    istore 1                 ; read y; 
    iload 0
    i2f
    iload 1
    i2f
    fdiv
    invokestatic gcd.print(F)V
    ldc " "
    invokestatic gcd.print(Ljava/lang/String;)V
    iload 0 
    iload 1  ; note : y is on top of x 
    jsr Lgcd 
    invokestatic gcd.print(I)V ; we have the result of gcd on the stack 
    ldc "\n"
    invokestatic gcd.print(Ljava/lang/String;)V
    return 
.end method 

