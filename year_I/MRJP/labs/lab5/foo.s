.global main


fib_rec:
  push    %ebp
  mov     %esp, %ebp

  mov     8(%ebp), %ecx
  mov     $1, %eax

  cmp     %eax, %ecx
  jle      _short_end_fib_rec

  dec     %ecx
  push    %ecx
  call    fib_rec
  pop     %ecx
  dec     %ecx
  push    %eax
  push    %ecx
  call    fib_rec
  add     $4, %esp
  pop     %ecx
  add     %ecx, %eax

  pop     %ebp
  ret
_short_end_fib_rec:
  mov     %ecx, %eax
  pop     %ebp
  ret


fib_it:
  mov     $0, %eax
  mov     $1, %edx

  mov     $0, %ecx

_fib_it_loop:
  cmp     %ecx, 4(%esp)
  jle     _fib_it_end

  add     %edx, %eax
  xchg    %edx, %eax
  inc     %ecx
  jmp     _fib_it_loop

_fib_it_end:
  ret



main:
  call	   readInt
  push     %eax
  call     fib_it
  push     %eax
  call     printInt
  add      $8, %esp

  mov     $0, %eax
  ret
