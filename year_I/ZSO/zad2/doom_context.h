#ifndef DOOM_CONTEXT_H
#define DOOM_CONTEXT_H

#include <linux/kref.h>
#include <linux/mutex.h>
#include "doom_common.h"

struct doom_context {
        struct doom_setup setup;
        /* Backreference to simplify code */
        struct doom_device *dev;

        struct mutex lock;
        struct kref kref;
};

struct doom_context *doom_context_create(void);
void get_doom_context(struct doom_context *);
void put_doom_context(struct doom_context *);
int get_doom_setup_safe(struct doom_context *, struct doom_setup *);

#endif /* DOOM_CONTEXT_H */
