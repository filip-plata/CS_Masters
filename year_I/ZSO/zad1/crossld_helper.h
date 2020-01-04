#ifndef CROSSLD_CROSSLD_HELPER_H
#define CROSSLD_CROSSLD_HELPER_H

#include <stdbool.h>
#include <elf.h>

#include "crossld.h"

struct crossld_state {
    void * stack_x32;
    void * __exec32;
    void * __exit;
    Elf32_Phdr * phdrs;
    int phdrs_num;
    void * trampolines;
    int ntrampolines;
} typedef crossld_state;

bool setup_stack_ptr(crossld_state *)
__attribute__ ((visibility ("hidden")));
bool setup_exec_x32_trampoline(crossld_state *)
__attribute__ ((visibility ("hidden")));
bool build_trampolines(const struct function *, int nfuncs,
                       crossld_state *)
__attribute__ ((visibility ("hidden")));
int cleanup(int, crossld_state)
__attribute__ ((visibility ("hidden")));
crossld_state initialize_state(int)
__attribute__ ((visibility ("hidden")));
bool build_exit_function(
        void**, uint64_t *, crossld_state *, struct function *)
__attribute__ ((visibility ("hidden")));
bool validate_input(const struct function *, int)
__attribute__ ((visibility ("hidden")));

#endif //CROSSLD_CROSSLD_HELPER_H
