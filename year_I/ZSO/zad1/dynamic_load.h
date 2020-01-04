#ifndef CROSSLD_DYNAMIC_LOAD_H
#define CROSSLD_DYNAMIC_LOAD_H

#include <stdbool.h>

#include "crossld_helper.h"
#include "crossld.h"

bool resolve_relocations(crossld_state *state, struct function *funcs)
__attribute__ ((visibility ("hidden")));

#endif //CROSSLD_DYNAMIC_LOAD_H
