.source hello.j
.class  public Fibonnaci
.super  java/lang/Object

;
; standard initializer
.method public <init>()V
   aload_0

   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method private static fibonnaci(I)J
.limit stack 4
.limit locals 5
  ; variables: smaller, bigger
  lconst_0
  lstore_1
  lconst_1
  lstore_3

  iload_0
  bipush 0
  if_icmple end

  loop:
    iload_0
    bipush 0
    if_icmpeq end
    iinc 0 -1
    lload_1
    lload_3
    ladd
    lload_3
    lstore_1
    lstore_3
    goto loop

  end:
    lload_3
    lreturn

.end method

.method public static main([Ljava/lang/String;)V
.limit stack 3
.limit locals 1
  getstatic java/lang/System/out Ljava/io/PrintStream;

  aload_0
  bipush 0
  aaload
  invokestatic java/lang/Integer/parseInt(Ljava/lang/String;)I

  invokestatic Fibonnaci/fibonnaci(I)J

  invokevirtual java/io/PrintStream/println(J)V
	return
.end method
