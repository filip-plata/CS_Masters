#include <linux/slab.h>

#include "doom_device.h"
#include "doom_dma.h"
#include "doom_fence.h"
#include "doom_setup.h"
#include "doom_command_wait.h"


void set_device_setup(struct doom_device *dev, struct doom_setup *setup)
{
        BUG_ON(!mutex_is_locked(&dev->lock));
        get_doom_setup(setup);
        put_doom_setup(&dev->dev_setup);
        dev->dev_setup = *setup;
}

#ifdef USE_CMD_BUF
int doom_device_allocate_cmd_buffer(struct doom_device *dev)
{
        dev->cmd_buf = create_command_buffer(
                dev, CMDS_QUEUE_SIZE * sizeof(struct doomdev2_cmd));

        if (IS_ERR(dev->cmd_buf))
                return PTR_ERR(dev->cmd_buf);
        return 0;
}

void doom_device_free_cmd_buffer(struct doom_device *dev)
{
        if (dev->cmd_buf)
                put_doom_buf(dev->cmd_buf);
        dev->cmd_buf = NULL;
}
#endif

void doom_dev_cleanup(struct doom_device **dooms, struct doom_device *dev)
{
        put_doom_setup(&dev->dev_setup);
        doom_free_if_unused(dooms, dev);
}

void doom_device_init(struct doom_device *dev)
{
        // we do not want to change used field value
        memset(dev, '\0', sizeof(struct doom_device));
        dev->used = true;
        dev->queue_space_cache = EFFECTIVE_QUEUE_SIZE;
        dev->cmds_count = 0;
        dev->open_count = 0;

        mutex_init(&dev->lock);
#ifdef USE_CMD_BUF
        dev->cmd_buffer_pos = 0;
#endif
        doom_async_queue_await_init(dev);

        INIT_LIST_HEAD(&dev->pending_batches);
        dev->fence_counter_cache = 0;
        spin_lock_init(&dev->fence_lock);
        init_waitqueue_head(&dev->fence_queue);
}

void doom_free_if_unused(struct doom_device **dooms, struct doom_device *dev)
{
        if (dev->used || dev->open_count != 0)
                return;
        dooms[dev->minor] = NULL;
        kfree(dev);
}

static void clear_batches_up_to_number(struct doom_device *dev, uint64_t num)
{
        struct doom_pending_batch *p_b;

        while (!list_empty(&dev->pending_batches)) {
                p_b = list_first_entry(&dev->pending_batches,
                        struct doom_pending_batch, list);
                if (p_b->batch_nr >= num)
                        break;

                list_del(&p_b->list);
                put_doom_setup(&p_b->setup);
                kfree(p_b);
        }
}

static void set_last_setup_batch_number(struct doom_device *dev)
{
        struct doom_pending_batch *p_b;

        if (list_empty(&dev->pending_batches))
                return;

        p_b = list_last_entry(&dev->pending_batches,
                struct doom_pending_batch, list);
        p_b->batch_nr = dev->sended_batches;
}

void perform_setup_bookkeeping(struct doom_device *dev,
        struct doom_pending_batch *p_batch)
{
        BUG_ON(!mutex_is_locked(&dev->lock));

        set_last_setup_batch_number(dev);
        refresh_fence_counter(dev);
        clear_batches_up_to_number(dev, dev->fence_counter_cache);
        list_add_tail(&p_batch->list, &dev->pending_batches);
}

void clear_all_batches(struct doom_device *dev)
{
        /* This method is for cleanup after setting device unused */
        BUG_ON(dev->used);
        /* Clear everything - well except for batch_nr -1 */
        clear_batches_up_to_number(dev, -1);
}
