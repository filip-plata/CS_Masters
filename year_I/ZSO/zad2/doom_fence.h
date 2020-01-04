#ifndef DOOM_FENCE_H
#define DOOM_FENCE_H

#include "doom_common.h"

void doom_fence_irq_handler(struct doom_device *);
int fence_await(struct doom_device *, uint64_t);
void refresh_fence_counter(struct doom_device *);

#endif /* DOOM_FENCE_H */
