#ifndef ASM_H
#define ASM_H

int extern __trampoline32();
int extern  __trampoline_args();
int extern __trampoline_exec32_arg();
int extern __trampoline_end();
void extern __exec32();
void extern __exec32_end();
void extern __exit();
void extern __exit_end();

#define __exec32_len (__exec32_end - __exec32)
#define __trampoline_len (__trampoline_end - __trampoline32)
#define __exit_len (__exit_end - __exit)

#endif /* ASM_H */