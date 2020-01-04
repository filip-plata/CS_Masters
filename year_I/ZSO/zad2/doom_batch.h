#ifndef DOOM_BATCH_H
#define DOOM_BATCH_H

#include <linux/uaccess.h>
#include <linux/slab.h>
#include "doom_common.h"

struct doom_batch {
        size_t size;
        struct doom_setup setup;
        void *data;
        struct cmd_format_all *first_copy_rect;
        struct doom_pending_batch *p_batch;
};

struct doom_batch_view {
        struct cmd_format_all *cmds;
        size_t cmds_count;
};

/* Call this function only under device lock. It changes device setup */
struct doom_batch_view obtain_batch_view(struct doom_device *,
        struct doom_batch *);

int doom_batch_init(struct doom_batch *, size_t, char __user *,
        struct doom_context *ctx);
void doom_batch_cleanup(struct doom_batch *);

#endif /* DOOM_BATCH_H */
