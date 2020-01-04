#ifndef CROSSLD_ELF_LOADER_H
#define CROSSLD_ELF_LOADER_H

#include <stdbool.h>
#include <elf.h>

bool load_xi386_elf(const char *fname, Elf32_Ehdr *elf,
                    Elf32_Phdr ** phdrs, int *phdrs_num)
__attribute__ ((visibility ("hidden")));

#endif //CROSSLD_ELF_LOADER_H
