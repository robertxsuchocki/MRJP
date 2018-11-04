.class public test
.super java/lang/Object
.method public static main([Ljava/lang/String;)V
.limit stack 5
.limit locals 3
bipush 42
istore_1
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_1
invokevirtual java/io/PrintStream/println(I)V
iload_1
iload_1
iload_1
imul
iload_1
iload_1
idiv
isub
iadd
istore_2
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
invokevirtual java/io/PrintStream/println(I)V
return
.end method
