#include <stdint.h>
#include <stdlib.h>

#include "cross_call.h"


size_t inline get_type_size_x32(enum type t)
__attribute__((always_inline, visibility("hidden")));

void inline calc_arg_values(size_t *args,
                            const struct function *fun, void *stack)
__attribute__((always_inline, visibility("hidden")));

extern uint64_t __call_function(const struct function * , uint64_t *);


int64_t __attribute__ ((noinline, used))
call_function(const struct function * fun, int(*exit_fun)(int),
              void * stack_args_x32) {
    /* Translate call to a 64bit version and do it,
     * then clean up and return. */
    uint64_t res;
    int n = fun->nargs;
    uint64_t args[n];

    calc_arg_values(args, fun, stack_args_x32 + 4);

    res = __call_function(fun, args);

    if ((fun->result == TYPE_PTR && res >= (1ULL << 32U)) ||
	(fun->result == TYPE_LONG && res >= (1ULL << 31U)) ||
	(fun->result == TYPE_LONG && (int64_t)res <= -(int64_t)(1ULL << 31U)) ||
	(fun->result == TYPE_UNSIGNED_LONG && res >= (1ULL << 32U)))
        exit_fun(-1);

    return res;
}

size_t get_type_size_x32(enum type t) {
    if (t == TYPE_LONG_LONG) return 8;
    if (t == TYPE_UNSIGNED_LONG_LONG) return 8;
    return 4;
}

void calc_arg_values(uint64_t *args, const struct function *fun,
                     void *stack)
{
    size_t off = 0;

    for (int i=0; i < fun->nargs; i++) {
        size_t type_size = get_type_size_x32((fun->args)[i]);
        if (type_size == 4)
            args[i] = *(uint32_t *) (stack + off);
        if (type_size == 8)
            args[i] = *(uint64_t *) (stack + off);
        off += type_size;
    }
}
