#include "doom_batch.h"
#include "doom_validate.h"
#include "doom_parse_cmd.h"
#include "doom_setup.h"
#include "doom_dma.h"
#include "doomdev2.h"
#include "doom_command_wait.h"

_Static_assert(sizeof (struct doomdev2_cmd) == sizeof(struct cmd_format_all),
        "Weird hacks in this module need such equality to be correct");

int build_batch_commands(
        struct doom_device *dev,
        struct doom_batch *batch)
{
        /* This validates a batch of commands and if it is okay,
         * parses it into the same buffer it has allocated for userspace
         * commands. */
        int i, err;
        unsigned int cmd_s = sizeof(struct cmd_format_all);
        struct cmd_format_all *curr_dev_cmd = batch->data;
        struct doomdev2_cmd *curr_cmd = batch->data + cmd_s;
        bool same_surf = batch->setup.surf_src == batch->setup.surf_dst;

        if ((err = validate_cmds_batch(&batch->setup, curr_cmd + 1, batch->size)))
                return err;

        for (i = 0; i < batch->size; i++) {
                curr_cmd++;
                curr_dev_cmd++;

                parse_command(&batch->setup, (struct cmd_format *) curr_dev_cmd, curr_cmd);
                if (curr_cmd->type != DOOMDEV2_CMD_TYPE_COPY_RECT)
                        continue;

                if (same_surf) {
                        curr_dev_cmd->interlock = true;
                }
                else {
                        if (batch->first_copy_rect == NULL)
                                batch->first_copy_rect = curr_dev_cmd;
                }
        }

        if (batch->first_copy_rect != NULL)
                batch->first_copy_rect->interlock = true;

        curr_dev_cmd->fence = true;
        return 0;
}

int doom_batch_init(struct doom_batch *batch, size_t size, char __user *buf,
        struct doom_context *ctx)
{
        long err = 0;
        unsigned int cmds_off = 2 * sizeof(struct cmd_format_all);
        batch->size = size;
        batch->first_copy_rect = NULL;
        batch->p_batch = NULL;

        batch->data = kmalloc(
                size * sizeof(struct doomdev2_cmd) + cmds_off, GFP_KERNEL);
        if (IS_ERR_OR_NULL(batch->data)) {
                err = -ENOMEM;
                goto err_alloc_cmd;
        }

        batch->p_batch = kmalloc(sizeof(struct doom_pending_batch), GFP_KERNEL);
        if (IS_ERR_OR_NULL(batch->data)) {
                err = -ENOMEM;
                goto err_alloc_pending_batch;
        }
        /* Some arbitrary large value - intent is to make this batch
         * live until another setup comes along. Implementation needs
         * this to be less than maximum uint64_t value. */
        batch->p_batch->batch_nr = 1ULL << 63;

        if ((err = copy_from_user(
                batch->data + cmds_off, buf, size * sizeof(struct doomdev2_cmd)))) {
                err = -EFAULT;
                goto err_copy_from_user;
        }

        if ((err = get_doom_setup_safe(ctx, &batch->setup)))
                goto err_get_setup;

        if ((err = build_batch_commands(ctx->dev, batch)))
                goto err_build_batch;

        err_build_batch:
        if (IS_ERR_VALUE(err))
                put_doom_setup(&batch->setup);
        err_get_setup:
        err_copy_from_user:
        if (IS_ERR_VALUE(err))
                kfree(batch->p_batch);
        err_alloc_pending_batch:
        if (IS_ERR_VALUE(err))
                kfree(batch->data);
        err_alloc_cmd:
        return err;
}

static bool setup_change_needed(
        struct doom_device *dev,
        struct doom_setup *setup,
        struct cmd_setup_format *cmd_f)
{
        bool setup_different;

        parse_command_setup(cmd_f, &dev->dev_setup, setup);
        // following line checks if diff setup command has any
        // bufor to be changed set. I happens to be one byte at offset
        // one. If it is zero, no changes are needed.
        setup_different = *(((char *) cmd_f) + 1);
        if (setup_different)
                set_device_setup(dev, setup);
        return setup_different;
}

static void ping_async_setup(struct doom_device *dev,
        struct cmd_format_all *cmds, size_t cmds_c)
{
        uint32_t freq = CMDS_QUEUE_SIZE >> FREE_PLACE_FREQ;
        uint32_t i = (freq - dev->cmds_count) % freq;

        while (i < cmds_c) {
                cmds[i].ping_async = true;
                i += freq;
        }

        dev->cmds_count += cmds_c;
        dev->cmds_count %= freq;
}

struct doom_batch_view obtain_batch_view(struct doom_device *dev,
        struct doom_batch *batch)
{
        bool do_setup;
        struct doom_batch_view result;

        do_setup = setup_change_needed(dev, &batch->setup,
                (struct cmd_setup_format *) batch->data);
        if (do_setup) {
                result.cmds = batch->data;
                result.cmds_count = batch->size + 1;

                batch->p_batch->setup = batch->setup;
                perform_setup_bookkeeping(dev, batch->p_batch);
                batch->p_batch = NULL;
        }
        else {
                result.cmds = batch->data + sizeof(struct cmd_format_all);
                result.cmds_count = batch->size;
                put_doom_setup(&batch->setup);
        }

        ping_async_setup(dev, result.cmds, result.cmds_count);

        return result;
}

void doom_batch_cleanup(struct doom_batch *batch)
{
        kfree(batch->data);
        if (batch->p_batch != NULL)
                kfree(batch->p_batch);
}
