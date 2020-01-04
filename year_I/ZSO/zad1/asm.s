.global __trampoline32
.global __trampoline_args
.global __trampoline_exec32_arg
.global __trampoline_end
.global __exec32
.global __exec32_end
.global __exit
.global __exit_end
.global __call_function

.hidden __trampoline32
.hidden __trampoline_args
.hidden __trampoline_exec32_arg
.hidden __trampoline_end
.hidden __exec32
.hidden __exec32_end
.hidden __exit
.hidden __exit_end
.hidden __call_function

.text

__call_function:
    # just translate ABI and call x64 function
    # -8 rbp is struct function *
    # -16 rbp is args table
    # -24 rbp is number n
    # -32 rbp is address of code we want to call
    # r10 is n of args
    # r11 is arguments table addr
    push %rbp
    mov %rsp, %rbp
    push %rdi
    push %rsi

    sub $16, %rsp

    # store nargs in r10 and stack
    lea 16(%rdi), %rax
    mov (%rax), %eax
    mov %rax, %r10
    mov %r10, -24(%rbp)

    # store code to call on stack
    lea 24(%rdi), %rax
    mov (%rax), %rax
    mov %rax, -32(%rbp)

    # address of args table in rcx
    mov -16(%rbp), %r11

    # prepare arguments on the stack
    mov %r10, %rcx
    mov $6, %rax

__set_stack_args:
    cmp %rax, %rcx
    jle __set_arg6
    # align stack if n is odd
    mov $1, %rdx
    test %r10, %rdx
    jz __set_stack_arg
    sub $8, %rsp
__set_stack_arg:
    cmp %rax, %rcx
    je __set_arg6
    dec %rcx
    lea (%r11, %rcx, 8), %rdx
    push (%rdx)
    jmp __set_stack_arg

__set_arg6:
    mov $6, %rax
    cmp %rax, %r10
    jl __set_arg5
    mov 40(%r11), %r9
__set_arg5:
    mov $5, %rax
    cmp %rax, %r10
    jl __set_arg4
    mov 32(%r11), %r8
__set_arg4:
    mov $4, %rax
    cmp %rax, %r10
    jl __set_arg3
    mov 24(%r11), %rcx
__set_arg3:
    mov $3, %rax
    cmp %rax, %r10
    jl __set_arg2
    mov 16(%r11), %rdx
__set_arg2:
    mov $2, %rax
    cmp %rax, %r10
    jl __set_arg1
    mov 8(%r11), %rsi
__set_arg1:
    mov $1, %rax
    cmp %rax, %r10
    jl __call_code
    mov (%r11), %rdi
__call_code:
    mov -32(%rbp), %rax
    call *%rax

    # Reclaim stack arguments
    mov -24(%rbp), %rcx
    sub $6, %rcx
    mov $0, %rdx
    cmp %rdx, %rcx
    jle __call_function_exit
    # reclaim padding
    mov $1, %rdx
    test %rcx, %rdx
    jz __reclaim_args
    add $8, %rsp
__reclaim_args:
    sal $3, %rcx # every argument takes 8 bytes
    add %rcx, %rsp

__call_function_exit:
    # cleanup of metadata and ret
    add $32, %rsp
    pop %rbp
    ret

.code32
__trampoline32:
    mov %esp, %edx
    sub $4, %esp    #align stack for x64 bit, stack is -4 % 16
    pushl $0x33
    call next
    next: pop %eax
    lea 6(%eax), %eax # 6 is magic number
    pushl %eax
    lret

    .code64
    push %rdi
    push %rsi
__trampoline_args:
    movabs $0x0, %rdi
    movabs $0x0, %rsi
    movabs $0x0, %rax
    call *%rax
    mov %rax, %rdx
    sar $32, %rdx 	
    pop %rsi
    pop %rdi
    subq $8, %rsp
    movl $0x23, 4(%rsp)
__trampoline_exec32_arg:
    movabs $0x0, %rcx
    movl %ecx, (%rsp)
    lea 1(%rip), %rcx
    lret
    .code32;
    add $4, %esp #  stack align for x32 bit
    ret
    .code64
__trampoline_end:


# Switch to x32 mode, jump to ecx

.code32
__exec32:
pushl $0x2b
popl %ds
pushl $0x2b
popl %es
jmp *%ecx
__exec32_end:
.code64;

# pointer to address to return to in crossld
# will replace 0x0
__exit:
movabs $0x0, %rcx;
movabs $0x0, %rbp;
mov (%rcx), %rcx;
mov (%rbp), %rbp;
mov %rdi, %rax;
jmp *%rcx;
__exit_end:
