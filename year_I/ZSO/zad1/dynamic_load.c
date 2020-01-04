#include <stddef.h>
#include <elf.h>
#include <string.h>

#include "dynamic_load.h"
#include "asm.h"

struct dyn_info {
    void * strtab;
    void * symtab;
    void * jmprel;
    size_t plt_size;
} typedef dyn_info;


static bool parse_dynamic_segment_info(Elf32_Phdr * phdr, dyn_info * d) {
    int info_count = phdr->p_memsz / sizeof(Elf32_Dyn);
    d->jmprel = NULL, d->symtab = NULL, d->strtab = NULL, d->plt_size = 0;

    Elf32_Dyn * cur_dyn = (Elf32_Dyn *) (intptr_t) phdr->p_vaddr;

    for (int j = 0; j < phdr->p_memsz / sizeof(Elf32_Dyn); j++) {
        if (cur_dyn->d_tag == DT_STRTAB) {
            d->strtab = (void *) (intptr_t) cur_dyn->d_un.d_ptr;
        }
        else if (cur_dyn->d_tag == DT_SYMTAB) {
            d->symtab = (void *) (intptr_t) cur_dyn->d_un.d_ptr;
        }
        else if (cur_dyn->d_tag == DT_PLTRELSZ) {
            d->plt_size = (size_t) cur_dyn->d_un.d_val;
        }
        else if (cur_dyn->d_tag == DT_JMPREL) {
            d->jmprel = (void *) (intptr_t) cur_dyn->d_un.d_ptr;
        }
        else {
            info_count--;
        }
        cur_dyn++;
    }

    return info_count <= 4;
}

static bool setup_function_symbol(char * symbol_name, struct function * funcs,
                            crossld_state * state,
                           uint32_t* symbol_addr) {

    for (int i =0; i< state->ntrampolines; i++) {
        if (strcmp(symbol_name, funcs[i].name) == 0) {
            *symbol_addr = (uint32_t) (intptr_t) (state->trampolines +
                    i * __trampoline_len);
            return true;
        }
    }

    return false;
}

bool resolve_relocations(crossld_state *state, struct function *funcs) {
    // addresses in state->trampolines + i *__trampoline_len
    // names in funcs[i].name

    int resolved_relocs = 0;
    dyn_info d;

    for (int i = 0; i < state->phdrs_num; i++) {
        Elf32_Phdr * c_hdr = state->phdrs + i;

        if (c_hdr->p_type != PT_DYNAMIC) continue;

        if (c_hdr->p_memsz % sizeof(Elf32_Dyn) != 0) return false; /* !? */

        if (!parse_dynamic_segment_info(c_hdr, &d)) return false; /* !? */

        Elf32_Sym  * c_sym = (Elf32_Sym *) d.symtab;

        for (int j = 0; j < (d.plt_size / sizeof(Elf32_Rel)); j++) {
            Elf32_Rel * plt = ((Elf32_Rel *) d.jmprel) + j;
            if (ELF32_R_TYPE(plt->r_info) != R_386_JMP_SLOT)
                continue;

            if (!setup_function_symbol(
                    d.strtab + (c_sym + ELF32_R_SYM(plt->r_info))->st_name, funcs,
                    state,(uint32_t *) (intptr_t) (plt->r_offset)))
                return false;
            resolved_relocs++;

        }
    }

    // exit can remain unplugged into relocs table,
    // because it was not used by 32 bit program
    return resolved_relocs >= state->ntrampolines - 1;
}
