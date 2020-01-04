#define _GNU_SOURCE
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <errno.h>
#include <string.h>
#include <sys/wait.h>

#define STACK_SIZE (1024 * 1024)
#define LIMIT 1000

const char* a_buf = "A\n";
const char* b_buf = "B\n";

void do_wait(pid_t pid_) {
  register int sys_nr __asm__("eax") = __NR_wait4;
  register ssize_t res __asm__("rax");
  register pid_t pid __asm__("rdi") = pid_;
  register int * stat_addr __asm__("rsi") = 0;
  register int options __asm__("rdx") = 0;

  __asm__ volatile (
    "syscall"
    :
    : "g" (sys_nr)
    : "cc", "rdi", "rsi", "rdx", "memory"
  );
}

void do_exit(int code_) {
  register int sys_nr __asm__("eax") = __NR_exit;
  register int code __asm__("rdi") = code_;
  __asm__ volatile (
    "syscall"
    :
    : "g" (sys_nr)
    : "cc", "rdi", "memory"
  );
}

ssize_t do_write(int fd_, const char * buf_, size_t count_) {
  register int sys_nr __asm__("eax") = __NR_write;
  register ssize_t res __asm__("rax");
  register int fd __asm__("rdi") = fd_;
  register const char * buf __asm__("rsi") = buf_;
  register size_t count __asm__("rdx") = count_;
  __asm__ volatile (
    "syscall"
    : "=g" (res)
    : "g" (sys_nr), "g" (fd), "g" (buf), "g" (count)
    : "cc", "memory"
  );
  return res;
}

int child_write() {
    for (int j = 0; j < LIMIT; j++) do_write(1, a_buf, 2);
    do_exit(0);
}

pid_t do_clone(unsigned long clone_flags_, unsigned long newsp_) {
  register unsigned long sys_nr __asm__("rax") = 56;
  register pid_t res __asm__("rax");
  register unsigned long clone_flags __asm__("rdi") = clone_flags_;
  register unsigned long newsp __asm__("rsi") = newsp_;

  __asm__ volatile (
    "syscall;\n\t"\
    "cmp $0, %%rax;\n\t"\
    "jne %=f;\n\t"\
    "call child_write;\n\t"\
    "%=:\n\t"\
    : "=g" (res)
    : "g" (sys_nr), "g" (clone_flags), "g" (newsp)
    : "cc", "memory"
  );

  return res;
}

int main(int argc, char *argv[]) {
    char* stack, * stackTop;
    int flags = (CLONE_THREAD | CLONE_SIGHAND | CLONE_VM | 0);
    stack = malloc(STACK_SIZE);
    stackTop = stack + STACK_SIZE;
    
    pid_t pid = do_clone(flags, (unsigned long) stackTop);

    if (pid < 0) do_exit(127);

    for (int i = 0; i < LIMIT; i++) do_write(1, b_buf, 2);

    do_wait(pid);
    do_exit(0);
}
