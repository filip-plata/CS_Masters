#ifndef DOOM_COMMAND_WAIT_H
#define DOOM_COMMAND_WAIT_H

#include "doom_common.h"

#define FREE_PLACE_FREQ 3

_Static_assert(CMDS_QUEUE_SIZE % (1 << FREE_PLACE_FREQ) == 0, "");

void doom_async_queue_await_init(struct doom_device *);
void doom_pong_async(struct doom_device *);
int wait_for_commands_queue(struct doom_device *, size_t);
void unreserve_queue_space(struct doom_device *, size_t);

#endif /* DOOM_COMMAND_WAIT_H */
