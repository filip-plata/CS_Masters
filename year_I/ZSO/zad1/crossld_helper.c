#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include <sys/mman.h>
#include <stdlib.h>

#include "crossld_helper.h"
#include "cross_call.h"
#include "asm.h"
#include "utils.h"

#define STACK_SIZE (5 * 1024 * 1024)
#define STACK_RANGE (STACK_SIZE - 0)
#define WORD_SIZE 8

static const char __exit_name[] = "exit";

bool build_trampolines(const struct function *fun, int nfuncs,
                       crossld_state *state) {
    size_t arg_off = (size_t) (__trampoline_args - __trampoline32) + 2;
    size_t code_size = nfuncs * __trampoline_len;

    void * call_func_addr = call_function;
    void * p = mmap(NULL, code_size,
                    PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT, -1, 0);

    if (p == MAP_FAILED) return false;

    for (int i = 0; i < nfuncs; i++) {
        void * base = p + i * __trampoline_len;
        const struct function * func = fun + i;

        memcpy(base, __trampoline32, __trampoline_len);
        memcpy(base + arg_off, &func, WORD_SIZE);
        memcpy(base + arg_off + 10, &(state->__exit), WORD_SIZE);
        memcpy(base + arg_off + 20, &call_func_addr, WORD_SIZE);
        memcpy(base + 2 + (__trampoline_exec32_arg - __trampoline32),
                &(state->__exec32), WORD_SIZE);
    }

    if (mprotect(p, code_size, PROT_EXEC | PROT_READ) != 0)
        return false;

    state->trampolines = p;

    return true;
}

static int comp(const void* a, const void* b) {
    const char* pa = *(const char**)a;
    const char* pb = *(const char**)b;

    return strcmp(pa, pb);
}

bool validate_input(const struct function * funcs, int nfuncs) {
    int i;
    const char* names[nfuncs];

    if (nfuncs < 0) return false;

    for (i = 0; i < nfuncs; i++)
        names[i] = funcs[i].name;

    for (i = 0; i < nfuncs; i++)
        if (strcmp(funcs[i].name, __exit_name) == 0) return false;

    qsort(names, (size_t) nfuncs, sizeof(char*), comp);

    for (i = 1; i < nfuncs; i++)
        if (strcmp(names[i], names[i-1]) == 0) return false;

    return true;
}

bool build_exit_function(
        void** ret_addr,
        uint64_t * rbp,
        crossld_state * state,
        struct function * fun) {
    static const enum type t = TYPE_INT;

    state->__exit = mmap(NULL, __exit_len, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT, -1, 0);

    if (state->__exit == MAP_FAILED) return false;

    size_t off = 2;
    size_t arg_off = 10;

    memcpy(state->__exit, __exit, __exit_len);
    memcpy(state->__exit + off, &ret_addr, WORD_SIZE);
    off += arg_off;
    memcpy(state->__exit + off, &rbp, WORD_SIZE);

    if (mprotect(state->__exit, __exit_len, PROT_READ | PROT_EXEC) != 0)
        return false;

    fun->result = TYPE_VOID;
    fun->name = __exit_name;
    fun->nargs = 1;
    fun->code = state->__exit;
    fun->args = &t;

    return true;
}

bool setup_stack_ptr(crossld_state *state) {
    if ((state->stack_x32 = mmap(NULL, STACK_SIZE, PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT,
                                 -1, 0)) == MAP_FAILED)
        return false;
    state->stack_x32  = (void *)
            (((uint64_t) state->stack_x32) + STACK_RANGE);
    return true;
}

static void * create_exec32_copy() {
    void * copy_addr;

    copy_addr = mmap(NULL, __exec32_len, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_32BIT, -1, 0);
    if (copy_addr == MAP_FAILED)
        return MAP_FAILED;
    memcpy(copy_addr, __exec32, __exec32_len);
    if (mprotect(copy_addr, __exec32_len, PROT_EXEC) != 0)
        return false;

    return copy_addr;
}

bool setup_exec_x32_trampoline(crossld_state * state) {
    if ((state->__exec32 = create_exec32_copy()) == MAP_FAILED)
        return false;
    return true;
}

static void unmap_if_valid(void * ptr, size_t size) {
    if (ptr != NULL && ptr != MAP_FAILED)
        munmap(ptr, size);
}

int cleanup(int stat, crossld_state state) {
    if (state.stack_x32 != NULL && state.stack_x32 != MAP_FAILED)
        munmap(state.stack_x32 - STACK_RANGE, STACK_SIZE);
    unmap_if_valid(state.__exec32, __exec32_len);
    unmap_if_valid(state.__exit, __exit_len);
    unmap_if_valid(state.trampolines,
                   state.ntrampolines * __trampoline_len);

    if (state.phdrs != NULL) {
	/* We maintain invariant that if state.phdrs is not
	 * null, all PT_LOAD segments are loaded */
        Elf32_Phdr * c_hdr = NULL;

        for (int i=0; i< state.phdrs_num; i++) {
            c_hdr = state.phdrs + i;

            if (c_hdr->p_type != PT_LOAD) continue;

            intptr_t aligned_addr =
                    get_aligned_addr((intptr_t) c_hdr->p_vaddr);
            munmap((void *) aligned_addr, (size_t)
                   (c_hdr->p_vaddr - aligned_addr) + c_hdr->p_memsz);
        }

        free(state.phdrs);
    }

    return stat;
}

crossld_state initialize_state(int nfuncs) {
    return  (crossld_state)
            {.__exec32 = NULL, .phdrs = NULL,
             .trampolines = NULL, .stack_x32 = NULL,
             .ntrampolines = nfuncs + 1, .phdrs_num = 0,
             .__exit = NULL};
}
