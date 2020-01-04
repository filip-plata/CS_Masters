#include <stddef.h>
#include <memory.h>

#include "elf_loader.h"
#include "dynamic_load.h"
#include "crossld_helper.h"
#include "crossld.h"

#include "asm.h"

#define FAIL -1

typedef struct function function;

#define __cleanup(X) (cleanup(X, state))

int crossld_start(const char *fname, const function *funcs, int nfuncs) {
    int res;
    Elf32_Ehdr elf_h;
    void * start_addr, * return_addr;
    uint64_t rbx, rbp, rsp, r12, r13, r14, r15;
    struct function funcs_exit[nfuncs + 1];

    crossld_state state = initialize_state(nfuncs);

    if (!validate_input(funcs, nfuncs)) return __cleanup(FAIL);

    memcpy((void *)funcs_exit, funcs, nfuncs * sizeof(function));
    if(!build_exit_function(
            &return_addr, &rbp, &state, funcs_exit + nfuncs))
        return __cleanup(FAIL);

    if (!load_xi386_elf(fname, &elf_h, &(state.phdrs), &(state.phdrs_num)))
        return __cleanup(FAIL);

    if (!setup_exec_x32_trampoline(&state))
        return __cleanup(FAIL);
    if (!build_trampolines(funcs_exit, nfuncs + 1, &state))
        return __cleanup(FAIL);

    if (!setup_stack_ptr(&state)) return __cleanup(FAIL);

    if (!resolve_relocations(&state, funcs_exit))
        return __cleanup(FAIL);
    start_addr = (void *) (intptr_t) elf_h.e_entry;

    /* save state, set new stack, jump to code fixing ds and es, then
     * to new program */
    __asm__ volatile(
        "mov %%rbx, %0;\n"
        "mov %%rbp, %1;\n"
        "mov %%r12, %2;\n"
        "mov %%r13, %3;\n"
        "mov %%r14, %4;\n"
        "mov %%r15, %5;\n"
        "mov %%rsp, %6;\n"
        "movq %9, %%rsp;\n"
        "subq $8, %%rsp;\n"
        "movl $0x23, 4(%%rsp);\n"
        "mov %10, %%rax;\n"
        "movl %%eax, (%%rsp);\n"
        "mov %11, %%rcx;\n"
        "lea 8(%%rip), %%rax;\n" // lea go get next instruction after lret
        "mov %%rax, %7;\n"
        "lret;\n"
        "mov %0, %%rbx;\n"
        "mov %2, %%r12;\n"
        "mov %3, %%r13;\n"
        "mov %4, %%r14;\n"
        "mov %5, %%r15;\n"
        "mov %6, %%rsp;\n"
        "mov %%rax, %8;\n"
        : "=m" (rbx), "=m" (rbp), "=m" (r12), "=m" (r13),
          "=m" (r14), "=m" (r15), "=m" (rsp), "=m" (return_addr), "=m" (res)
        : "g" (state.stack_x32), "g" (state.__exec32), "g" (start_addr)
        : "cc", "memory", "rax", "rbx", "rcx", "rdx", "rsi", "rdi",
          "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    );

    return __cleanup(res);
}

#undef __cleanup
