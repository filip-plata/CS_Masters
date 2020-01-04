#include <stddef.h>
#include <unistd.h>
#include <string.h>

#include <sys/mman.h>
#include <fcntl.h>
#include <stdlib.h>

#include "utils.h"
#include "elf_loader.h"

#define PT_LOADED 127
#define ELF32_Ehdr_s sizeof(Elf32_Ehdr)

#define ELF_TO_MPROT_FLAGS(FLAG) ( \
    ((FLAG & PF_X) ? PROT_EXEC : 0) |\
    ((FLAG & PF_W) ? PROT_WRITE : 0) |\
    ((FLAG & PF_R) ? PROT_READ : 0) \
    )

static bool validate_xi386_elf(Elf32_Ehdr *elf) {
    if (memcmp(elf->e_ident, ELFMAG, strlen(ELFMAG)) != 0) return false;
    if (elf->e_ident[4] != ELFCLASS32) return false;
    if (elf->e_machine != EM_386) return false;
    if (elf->e_type != ET_EXEC) return false;
    if (elf->e_version != EV_CURRENT) return false;

    return true;
}


static bool read_xi386_elf(int fd, Elf32_Ehdr *elf,
                    Elf32_Phdr ** phdrs, int *phdrs_num) {

    /* This modifies elf header and program headers to have
     * real addresses under which they are loaded in memory
     * and a real entry point. */

    int i;
    const unsigned char elf_hdr[ELF32_Ehdr_s];
    if (read(fd, (void *)elf_hdr, ELF32_Ehdr_s) != ELF32_Ehdr_s) return false;
    *elf = *((Elf32_Ehdr *) elf_hdr);

    if (!validate_xi386_elf(elf)) return false;

    size_t phs = elf->e_phentsize;
    lseek(fd, elf->e_phoff, SEEK_SET);
    *phdrs_num = elf->e_phnum;
    *phdrs = (Elf32_Phdr *) malloc(*phdrs_num * sizeof(Elf32_Phdr));

    for (i = 0; i< *phdrs_num; i++)
        (*phdrs + i)->p_type = PT_NULL;

    const unsigned char * phdr_buffer[phs];

    for (i = 0; i< elf->e_phnum; i++) {
        if (read(fd, (void *) phdr_buffer, phs) != phs) {
            free(*phdrs);
            *phdrs = NULL;
            return false;
        }
        (*phdrs)[i] = *((Elf32_Phdr *) phdr_buffer);
    }

    return true;
}

static bool mmap_execs(int fd, Elf32_Phdr * phdrs, int phdrs_num) {
    bool is_success = true;

    for (int i = 0; i< phdrs_num; i++) {
        Elf32_Phdr * c_hdr = phdrs + i;
        if (c_hdr->p_type != PT_LOAD) continue;

        intptr_t aligned_addr = get_aligned_addr((intptr_t) c_hdr->p_vaddr);
	    size_t size = ((size_t) c_hdr->p_vaddr - aligned_addr)
	            + c_hdr->p_memsz;
        void * res = mmap(
                (void *) (intptr_t) aligned_addr,
                size,
                PROT_WRITE, MAP_PRIVATE | MAP_32BIT | MAP_ANONYMOUS,
                -1, 0);

        if (res == MAP_FAILED || (intptr_t) res != aligned_addr) {
	    // we cannot do reliable unloading of data on cleanup
            if ((intptr_t) res != aligned_addr)
                munmap(res, size);
            is_success = false;
            break;
        }

        c_hdr->p_type = PT_LOADED;

        if (lseek(fd, c_hdr->p_offset, SEEK_SET) < 0 ||
            read(fd, (void *) (intptr_t)
                 c_hdr->p_vaddr, c_hdr->p_filesz) != c_hdr->p_filesz) {
            is_success = false;
            break;
        }

        if (mprotect(res, c_hdr->p_memsz,
                ELF_TO_MPROT_FLAGS(c_hdr->p_flags)) != 0) {
            is_success = false;
            break;
        }
    }

    for (int i = 0; i < phdrs_num; i++) {
        Elf32_Phdr * c_hdr = phdrs + i;
        if (c_hdr->p_type != PT_LOADED) continue;

        if (!is_success) {
            intptr_t addr = (intptr_t) c_hdr->p_vaddr;
            intptr_t aligned_addr = get_aligned_addr(addr);
            munmap((void *) aligned_addr,
                   (size_t) (addr - aligned_addr) + c_hdr->p_memsz);
        }
        /* Reset header type */
        c_hdr->p_type = PT_LOAD;
    }

    return is_success;
}

bool load_xi386_elf(const char *fname, Elf32_Ehdr *elf,
                    Elf32_Phdr ** phdrs, int *phdrs_num) {
    int fd;
    if ((fd = open(fname, O_RDONLY)) == -1) return false;

    if (!read_xi386_elf(fd, elf, phdrs, phdrs_num)) {
        close(fd);
        return false;
    }

    if(!mmap_execs(fd, *phdrs, *phdrs_num)) {
        /* If phdrs not null, they are assumed to be fully
         * loaded into memory */
	    free(*phdrs);
	    *phdrs = NULL;
	    close(fd);
	    return false;
    }

    if (close(fd) != 0) return false;
    return true;
}

#undef ELF_TO_MPROT_FLAGS
