.class public Print
.super java/lang/Object

.method public <init>()V
  .limit stack 1
  .limit locals 1
  0: aload_0
  1: invokespecial java/lang/Object/<init>()V
  4: return
.end method

.method public static main([Ljava/lang/String;)V
  .limit stack 2
  .limit locals 1
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: iconst_0
  4: invokevirtual java/io/PrintStream/println(I)V
  7: return
.end method
