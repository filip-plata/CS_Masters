#include <linux/slab.h>

#include "doom_context.h"
#include "doom_setup.h"

void static doom_context_release(struct kref *kref)
{
        struct doom_context *ctx = container_of(kref, struct doom_context, kref);
        put_doom_setup(&ctx->setup);
        kfree(ctx);
}

int get_doom_setup_safe(struct doom_context *ctx, struct doom_setup *setup)
{
        int err;
        if ((err = mutex_lock_interruptible(&ctx->lock)))
                return err;

        *setup = ctx->setup;
        get_doom_setup(setup);

        mutex_unlock(&ctx->lock);
        return 0;
}

void get_doom_context(struct doom_context *ctx) {
        kref_get(&ctx->kref);
}

void put_doom_context(struct doom_context *ctx) {
        kref_put(&ctx->kref, &doom_context_release);
}

struct doom_context * doom_context_create(void)
{
        struct doom_context *ctx = kmalloc(sizeof(struct doom_context), GFP_KERNEL);

        if (IS_ERR_OR_NULL(ctx)) {
                ctx = ERR_PTR(-ENOMEM);
                goto err_kmalloc;
        }

        memset(ctx, '\0', sizeof(struct doom_context));
        kref_init(&ctx->kref);
        mutex_init(&ctx->lock);

        err_kmalloc:
        return ctx;
}
