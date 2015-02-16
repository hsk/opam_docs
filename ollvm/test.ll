; ModuleID = 'name'
target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"


define i32 @fact(i1 %0) {
entry:
        %1 = icmp eq i1 %0, 0
        br i1 %1, label %then, label %else
then:
       ret i32 1
else:
       %2 = sub i1 %0, 1
       %3 = call i32 @fact(i32 %2)
       %4 = mul i1 %0, %3
       ret i32 %4
}
