#ifndef CROSSLD_CROSS_CALL_H
#define CROSSLD_CROSS_CALL_H

#include "crossld.h"

int64_t call_function(const struct function *, int(*)(int), void *)
__attribute__ ((visibility ("hidden")));

#endif //CROSSLD_CROSS_CALL_H
