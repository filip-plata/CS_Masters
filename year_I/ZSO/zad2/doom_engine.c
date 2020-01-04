#include <linux/kernel.h>
#include <linux/slab.h>
#include <linux/ioctl.h>
#include <linux/uaccess.h>

#include "doomdev2.h"
#include "doom_device.h"
#include "doom_context.h"
#include "doom_dma.h"
#include "doom_fence.h"
#include "harddoom_interface.h"
#include "doom_validate.h"
#include "doom_batch.h"
#include "doom_command_wait.h"
#include "doom_setup.h"

#define BATCH_SIZE_LIMIT 0xff
#define MAX_COMMANDS_BATCH min((EFFECTIVE_QUEUE_SIZE - 1), BATCH_SIZE_LIMIT)
#define SHOULD_EXPECT_OVERFLOW 0xffffffffULL


long setup_doom(struct doom_context *ctx,
                struct doomdev2_ioctl_setup *setup_ioctl)
{
        int err;
        struct doom_setup setup;

        setup.surf_dst = get_doom_surface(ctx->dev, setup_ioctl->surf_dst_fd);
        setup.surf_src = get_doom_surface(ctx->dev, setup_ioctl->surf_src_fd);
        setup.texture = get_doom_buffer(ctx->dev, setup_ioctl->texture_fd);
        setup.flat = get_doom_buffer(ctx->dev, setup_ioctl->flat_fd);
        setup.colormap = get_doom_buffer(ctx->dev, setup_ioctl->colormap_fd);
        setup.translation = get_doom_buffer(ctx->dev, setup_ioctl->translation_fd);
        setup.tranmap = get_doom_buffer(ctx->dev, setup_ioctl->tranmap_fd);

        if ((err = validate_setup_cmd(&setup)))
                goto err_validate;

        if ((err = mutex_lock_interruptible(&ctx->lock)))
                goto err_mutex_lock;

        /* Reference counts are bumped by get methods */
        put_doom_setup(&ctx->setup);
        ctx->setup = setup;

        mutex_unlock(&(ctx->lock));
        err_mutex_lock:
        err_validate:
        return err;
}

static long doom_ioctl_create_surface(struct doom_context *ctx, void __user * arg)
{
        int err;
        struct doom_surface *srf;
        struct doomdev2_ioctl_create_surface kbuf;

        if ((err = copy_from_user(&kbuf, (void __user *) arg,
                sizeof(struct doomdev2_ioctl_create_surface))))
                return err;
        srf = create_surface(ctx->dev, kbuf);
        if (IS_ERR_OR_NULL(srf))
                return PTR_ERR(srf);

        return get_doom_buf_fd(srf->buf);
}

static long doom_ioctl_create_buffer(struct doom_context *ctx, void __user *arg)
{
        int err;
        struct doom_buffer *buf;
        struct doomdev2_ioctl_create_buffer kbuf;

        if ((err = copy_from_user(&kbuf, (void __user *) arg,
                       sizeof(struct doomdev2_ioctl_create_buffer))))
                       return err;
        buf = create_buffer(ctx->dev, kbuf);
        if (IS_ERR_OR_NULL(buf))
                return PTR_ERR(buf);

        return get_doom_buf_fd(buf->buf);
}

static long doom_ioctl_setup(struct doom_context *ctx, void __user *arg)
{
        int err;
        struct doomdev2_ioctl_setup kbuf;

        if ((err = copy_from_user(&kbuf, (void __user *) arg,
                       sizeof(struct doomdev2_ioctl_setup))))
                       return -1;
        return setup_doom(ctx, &kbuf);
}

long doom_ioctl(struct file *file, unsigned int cmd, unsigned long arg)
{
        long res;
        struct doom_context *ctx = file->private_data;

        if (unlikely(!ctx->dev->used))
                return -EIO;

        switch (cmd) {
        case DOOMDEV2_IOCTL_CREATE_SURFACE:
                res = doom_ioctl_create_surface(ctx, (void __user *) arg);
                break;
        case DOOMDEV2_IOCTL_CREATE_TEXTURE:
                res = doom_ioctl_create_buffer(ctx, (void __user *) arg);
                break;
        case DOOMDEV2_IOCTL_SETUP:
                res = doom_ioctl_setup(ctx, (void __user *) arg);
                break;
        default:
                res = -EINVAL;
        }

        return res;
}

static void set_pending_batch(struct doom_raw_buf **buf, int64_t batch_nr)
{
        if (buf != NULL)
                set_pending_batch_number(*buf, batch_nr);
}

static void sending_new_batch(struct doom_device *dev, struct doom_setup *setup)
{
        /* This is called under device lock, so its safe */
        struct doom_raw_buf ***bufs = (struct doom_raw_buf ***) setup;
        int i;
        int64_t c = dev->sended_batches++;

        if (!dev->expecting_overflow &&
           (dev->sended_batches % (1ULL << 32) == SHOULD_EXPECT_OVERFLOW))
                dev->expecting_overflow = true;

        for (i = 0; i < 7; i++)
                set_pending_batch(bufs[i], c);
}

ssize_t do_doom_write(struct doom_context *ctx, const char __user *bufer, size_t cmds_c)
{
        long err = 0;
        struct doom_device *dev = ctx->dev;
        struct doom_batch batch;
        struct doom_batch_view batch_view;

        if ((err = doom_batch_init(&batch, cmds_c, (char __user *) bufer, ctx)))
                goto err_batch_init;

        if ((err = wait_for_commands_queue(dev, cmds_c + 1)))
                goto err_wait_command_queue;

        if ((err = mutex_lock_interruptible(&dev->lock)))
                goto err_device_lock;

        if (unlikely(!dev->used)) {
                err = -EIO;
                goto err_dev_removed;
        }

        batch_view = obtain_batch_view(dev, &batch);
        sending_new_batch(dev, &batch.setup);
        do_device_commands(dev, batch_view.cmds, batch_view.cmds_count);

        err_dev_removed:
        mutex_unlock(&dev->lock);
        err_device_lock:
        unreserve_queue_space(dev, cmds_c + 1);
        err_wait_command_queue:
        doom_batch_cleanup(&batch);
        err_batch_init:

        if (IS_ERR_VALUE(err))
                return err;
        else
                return batch.size;
}

ssize_t doom_write(struct file *file, const char __user *bufer, size_t count, loff_t *filepos)
{
        long res;
        struct doom_context *ctx = file->private_data;
        size_t passed = 0, next, cmd_s = sizeof(struct doomdev2_cmd);

        if (unlikely(count % cmd_s != 0))
                return -EINVAL;
        count /= cmd_s;

        while (passed < count) {
                next = MAX_COMMANDS_BATCH;
                if (next + passed > count)
                        next = count - passed;
                res = do_doom_write(ctx, bufer + passed * cmd_s, next);
                if (IS_ERR_VALUE(res)) {
                        if (passed == 0)
                                return res;
                        else
                                break;
                }
                passed += res;
        }

        return passed;
}
