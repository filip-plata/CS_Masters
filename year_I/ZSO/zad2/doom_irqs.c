#include <linux/pci.h>

#include "doom_dma.h"
#include "harddoom2.h"
#include "doom_irqs.h"

#define INTR_MASK_NO_PONG_ASYNC (HARDDOOM2_INTR_MASK & ~HARDDOOM2_INTR_PONG_ASYNC)

void inline enable_irqs(struct doom_device *dev)
{
        iowrite32(INTR_MASK_NO_PONG_ASYNC, dev->bar + HARDDOOM2_INTR_ENABLE);
}

void inline disable_irqs(struct doom_device *dev)
{
        iowrite32(0, dev->bar + HARDDOOM2_INTR_ENABLE);
}

void inline reset_irqs(struct doom_device *dev)
{
        iowrite32(HARDDOOM2_INTR_MASK, dev->bar + HARDDOOM2_INTR);
}

void inline reset_irq(struct doom_device *dev, uint32_t irq)
{
        iowrite32(irq, dev->bar + HARDDOOM2_INTR);
}

uint32_t active_irqs(struct doom_device *dev)
{
        return ioread32(dev->bar + HARDDOOM2_INTR);
}

void enable_pong_async_irq(struct doom_device *dev)
{
        iowrite32(HARDDOOM2_INTR_MASK, dev->bar + HARDDOOM2_INTR_ENABLE);
}

void disable_pong_async_irq(struct doom_device *dev)
{
        iowrite32(INTR_MASK_NO_PONG_ASYNC, dev->bar + HARDDOOM2_INTR_ENABLE);
}

irqreturn_t doom_irq_handler(int irq, void *v)
{
        struct doom_device *device = v;
        struct harddoom2_irq_callback *irq_h = device->irq_data;
        uint32_t irqs = active_irqs(device);
        int mask = 1, i;

        if (!irqs)
                return IRQ_NONE;

        PDEBUGG("Received interrupt: %d from device %d", irqs, device->minor);

        while (irqs) {
                switch (irqs & mask) {
                case HARDDOOM2_INTR_FENCE:
                        if (irq_h->fence)
                                irq_h->fence(device);
                        break;
                case HARDDOOM2_INTR_PONG_SYNC:
                        irq_h->pong_sync(device);
                        break;
                case HARDDOOM2_INTR_PONG_ASYNC:
                        irq_h->pong_async(device);
                        break;
                case HARDDOOM2_INTR_FE_ERROR:
                        printk(KERN_ERR "Invalid command received by doom "\
                         "device: %d", device->minor);
                        irq_h->fe_error(device);
                        break;
                case HARDDOOM2_INTR_SURF_DST_OVERFLOW:
                case HARDDOOM2_INTR_SURF_SRC_OVERFLOW:
                        printk(KERN_ERR "Surface overflow in "\
                        "device: %d", device->minor);
                        irq_h->surface_overflow(device);
                        break;
                case HARDDOOM2_INTR_CMD_OVERFLOW:
                        printk(KERN_ERR "Overflow of device internal queue: %d",
                        device->minor);
                        irq_h->cmd_overflow(device);
                        break;
                case HARDDOOM2_INTR_PAGE_FAULT_CMD:
                case HARDDOOM2_INTR_PAGE_FAULT_SURF_DST:
                case HARDDOOM2_INTR_PAGE_FAULT_SURF_SRC:
                case HARDDOOM2_INTR_PAGE_FAULT_TEXTURE:
                case HARDDOOM2_INTR_PAGE_FAULT_FLAT:
                case HARDDOOM2_INTR_PAGE_FAULT_TRANSLATION:
                case HARDDOOM2_INTR_PAGE_FAULT_COLORMAP:
                case HARDDOOM2_INTR_PAGE_FAULT_TRANMAP:
                        printk(KERN_ERR "Page fault %d on dev %d",
                               irqs, device->minor);
                        for (i = 0; i < 8 ; i++) {
                                PDEBUG("translation map %x, vaddr %x, tlb_entry %x",
                                        ioread32(device->bar + HARDDOOM2_TLB_PT(i)),
                                        ioread32(device->bar + HARDDOOM2_TLB_VADDR(i)),
                                        ioread32(device->bar + HARDDOOM2_TLB_ENTRY(i))
                                );
                        }
                        irq_h->page_fault(device);
                        break;
                default:
                        break;
                }

                irqs &= ~mask;
                mask <<= 1;
        }

        reset_irqs(device);

        return IRQ_HANDLED;
}
