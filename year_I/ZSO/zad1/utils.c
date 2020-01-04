#include <zconf.h>

#include "utils.h"

intptr_t get_aligned_addr(intptr_t addr) {
    int v = getpagesize();
    int align = 0;

    while (v >>= 1) align++;

    return (addr>> align) << align;
}
