#include <linux/jiffies.h>

#include "doom_command_wait.h"
#include "doom_irqs.h"
#include "harddoom_interface.h"
#include "harddoom2.h"
#include "debug.h"


static uint64_t queue_effective_space(struct doom_device *dev)
{
        return dev->queue_space_cache - dev->reserved_queue_space;
}

static void refresh_queue_space_cache(struct doom_device *dev)
{
        /* This number can only go up */
        dev->queue_space_cache = get_queue_free_place(dev);
}

void doom_async_queue_await_init(struct doom_device *dev)
{
        dev->pong_async_enabled = false;
        dev->waiting = 0;
        init_waitqueue_head(&dev->device_queue);
        spin_lock_init(&dev->device_queue_spinlock);
        dev->reserved_queue_space = 0;
}

void doom_pong_async(struct doom_device *dev)
{
        unsigned long flags;

        spin_lock_irqsave(&dev->device_queue_spinlock, flags);
        refresh_queue_space_cache(dev);
        spin_unlock_irqrestore(&dev->device_queue_spinlock, flags);

        wake_up_all(&dev->device_queue);
}

int wait_for_commands_queue(struct doom_device *dev, size_t cmds)
{
        long err = 0;
        unsigned long flags;

        spin_lock_irqsave(&dev->device_queue_spinlock, flags);

        while (queue_effective_space(dev) < cmds) {
                refresh_queue_space_cache(dev);

                if (queue_effective_space(dev) >= cmds)
                        break;

                if (!dev->pong_async_enabled) {
                        reset_irq(dev, HARDDOOM2_INTR_PONG_ASYNC);
                        enable_pong_async_irq(dev);
                        dev->pong_async_enabled = true;
                }
                dev->waiting++;
                spin_unlock_irqrestore(&dev->device_queue_spinlock, flags);

                /* timeout is for device disappearing from underneath */
                if ((err = wait_event_interruptible_timeout(
                        dev->device_queue, queue_effective_space(dev) >= cmds,
                        10 * HZ))) {
                        if (IS_ERR_VALUE(err))
                                goto err_wait_for_queue;
                }

                /* If the device has disappeard - no point in waiting */
                if (unlikely(!dev->used))
                        return -EIO;

                spin_lock_irqsave(&dev->device_queue_spinlock, flags);
                dev->waiting--;
        }

        dev->reserved_queue_space += cmds;
        if (dev->waiting) {
                wake_up_all(&dev->device_queue);
        }
        else {
                if (dev->pong_async_enabled) {
                        disable_pong_async_irq(dev);
                        dev->pong_async_enabled = false;
                }
        }
        spin_unlock_irqrestore(&dev->device_queue_spinlock, flags);

        err_wait_for_queue:
        return err;
}

void unreserve_queue_space(struct doom_device *dev, size_t cmds)
{
        unsigned long flags;

        spin_lock_irqsave(&dev->device_queue_spinlock, flags);
        dev->reserved_queue_space -= cmds;
        spin_unlock_irqrestore(&dev->device_queue_spinlock, flags);
}
