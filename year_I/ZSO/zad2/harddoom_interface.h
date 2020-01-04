#ifndef HARDDOOM_INTERFACE_H
#define HARDDOOM_INTERFACE_H

#include "doom_common.h"
#include "doom_parse_cmd.h"

int do_harddoom_init(struct doom_device *);
int do_harddoom_stop(struct doom_device *);
void do_device_commands(struct doom_device *, struct cmd_format_all *, size_t);
uint64_t get_queue_free_place(struct doom_device *);

#endif /* HARDDOOM_INTERFACE_H */
