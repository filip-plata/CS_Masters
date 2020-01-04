#include <linux/jiffies.h>

#include "doom_fence.h"
#include "doom_irqs.h"
#include "harddoom2.h"

#define OVRFLOW_THR 0x20000000


static inline uint32_t get_fence_counter(struct doom_device *dev)
{
        return ioread32(dev->bar + HARDDOOM2_FENCE_COUNTER);
}

static void refresh_fence_counter_unlocked(struct doom_device *dev)
{
        uint32_t f_c = get_fence_counter(dev);
        /* This modules detects overflows in fence counter. The only
         * reason for this is otherwise it would be impossible to wait
         * in reliable way, because number of packet we want to wait for
         * could get stale, and we would have to wait for a whole cycle. */

        if (dev->expecting_overflow && f_c < OVRFLOW_THR) {
                dev->overflows++;
                dev->expecting_overflow = false;
        }

        dev->fence_counter_cache = (dev->overflows << 32) + f_c;
}

void refresh_fence_counter(struct doom_device *dev)
{
        unsigned long flags;

        spin_lock_irqsave(&dev->fence_lock, flags);
        refresh_fence_counter_unlocked(dev);
        spin_unlock_irqrestore(&dev->fence_lock, flags);
}

static void set_fence_wait(struct doom_device *dev, uint64_t batch_nr)
{
        uint32_t wait = batch_nr % (1ULL << 32);
        iowrite32(wait, dev->bar + HARDDOOM2_FENCE_WAIT);
}

static int wait_for_batch(struct doom_device *dev, uint64_t batch_nr)
{
        long err = 0;
        unsigned long flags;

        spin_lock_irqsave(&dev->fence_lock, flags);

        while (batch_nr >= dev->fence_counter_cache) {
                reset_irq(dev, HARDDOOM2_INTR_FENCE);
                set_fence_wait(dev, batch_nr + 1);
                refresh_fence_counter_unlocked(dev);

                if (batch_nr < dev->fence_counter_cache)
                        break;

                spin_unlock_irqrestore(&dev->fence_lock, flags);

                /* After working with this device, I think pong sync
                 * interrupt was unreliable - that is, I could see fence
                 * counter was equal to my internal count of batches,
                 * but ping sync, which was set on the same command that had
                 * fence flag set, was not arriving.
                 * Thus, I add timeout here. In case interrupt fails
                 * to deliver. */
                if ((err = wait_event_interruptible_timeout(
                        dev->fence_queue,
                        batch_nr < dev->fence_counter_cache, 2 * HZ))) {
                        if (IS_ERR_VALUE(err))
                                goto err_fence_queue;
                }

                /* If the device has disappeard - no point in waiting */
                if (unlikely(!dev->used))
                        return -EIO;

                spin_lock_irqsave(&dev->fence_lock, flags);
        }

        spin_unlock_irqrestore(&dev->fence_lock, flags);
        /* At least one process will make progress out of while loop */
        wake_up_all(&dev->fence_queue);

        err_fence_queue:
        return err;
}

void doom_fence_irq_handler(struct doom_device *dev)
{
        refresh_fence_counter(dev);
        wake_up_all(&dev->fence_queue);
}

int fence_await(struct doom_device *dev, uint64_t batch_nr)
{
        if (batch_nr < dev->fence_counter_cache)
                return 0;
        refresh_fence_counter(dev);
        if (batch_nr < dev->fence_counter_cache)
                return 0;

        return wait_for_batch(dev, batch_nr);
}
