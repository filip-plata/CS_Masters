#include <stddef.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>

#include "make_formatter.h"

#define ADDR_SIZE 8

static const char* pattern =
        "\x55\x48\x89\xe5\x48\x89\xfe\x48\xbf\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x48\xb9\x00\x00\x00\x00\x00\x00\x00\x00\xff\xd1\x5d\xc3";
static const size_t pattern_length = 31;
static const size_t format_addr_offset = 9;
static const size_t printf_addr_offset = 19;

formatter make_formatter (const char * format) {
    void *p = mmap(NULL, 32, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    void *print_addr = printf;

    if (p == MAP_FAILED) return NULL;

    memcpy(p, pattern, pattern_length);
    memcpy(p + format_addr_offset, &format, ADDR_SIZE);
    memcpy(p + printf_addr_offset, &print_addr, ADDR_SIZE);
    mprotect(p, 32, PROT_NONE | PROT_EXEC);

    return p;
}

