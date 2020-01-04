#include "doom_reg.h"
#include "doom_irqs.h"
#include "doom_fence.h"
#include "doom_irq_handlers.h"
#include "harddoom_interface.h"
#include "doom_command_wait.h"

#define BAR0 0
#define BAR0_SIZE (1 << 13)
#define HARDDOOM_ADDR_SIZE 40

extern const char driver_name[];

static struct harddoom2_irq_callback irq_h = {
        .pong_async = doom_pong_async,
        .fence = doom_fence_irq_handler,
        .fe_error = doom_fe_error,
        .cmd_overflow = doom_cmd_overflow,
        .surface_overflow = doom_surface_overflow,
        .page_fault = doom_page_fault,
        .error = doom_error,
};

int doom_register_irqs(struct doom_device *dev)
{
        dev->irq_data = &irq_h;
        return request_irq(dev->pci_device->irq, doom_irq_handler,
                    IRQF_SHARED, "harddoom_irq", dev);
}

void doom_free_irqs(struct doom_device *dev)
{
        free_irq(dev->pci_device->irq, dev);
}

int pci_register_doom_device(struct pci_dev *pci_dev, struct doom_device *dev)
{
        int err;
        u64 mask = DMA_BIT_MASK(HARDDOOM_ADDR_SIZE);
        bool enabled_regions = false;
        dev->pci_device = pci_dev;
        pci_set_drvdata(pci_dev, dev);

        /* Order according to Documentation/pci.txt */
        if ((err = pci_enable_device(pci_dev)))
                goto err_pci_enable;

        pci_set_master(pci_dev);

        if ((err = pci_request_regions(pci_dev, driver_name)))
                goto err_pci_request_regions;

        enabled_regions = true;

        if ((err = pci_set_dma_mask(pci_dev, mask)))
                goto err_pci_set_dma_mask;

        if ((err = pci_set_consistent_dma_mask(pci_dev, mask)))
                goto err_pci_set_consistent_dma_mask;

        dev->bar = pci_iomap(pci_dev, BAR0, BAR0_SIZE);

        if (IS_ERR(dev->bar)) {
                err = PTR_ERR(dev->bar);
                goto err_pci_iomap;
        }

        if ((err = doom_register_irqs(dev)))
                goto err_register_irqs;

        return 0;

        err_register_irqs:
        pci_iounmap(pci_dev, dev->bar);
        err_pci_iomap:
        err_pci_set_consistent_dma_mask:
        err_pci_set_dma_mask:
        err_pci_request_regions:
        pci_disable_device(pci_dev);
        /* Documentation mentions a bug in kernel 2.6, which requires
         * this monkey releasing. No mention if they fixed it. */
        if (enabled_regions)
                pci_release_regions(pci_dev);
        pci_clear_master(pci_dev);
        err_pci_enable:
        return err;
}

void pci_deregister_doom_device(struct doom_device *dev)
{
        struct pci_dev *pci_dev = dev->pci_device;

        doom_free_irqs(dev);
        pci_clear_master(pci_dev);

        pci_iounmap(pci_dev, dev->bar);
        pci_release_regions(pci_dev);

        pci_disable_device(pci_dev);
}

int platform_register(
        struct doom_device *dev, dev_t doom_major,
        struct file_operations *doom_fops,
        struct class *doom_class)
{
        int err;

        cdev_init(&dev->doom_cdev, doom_fops);
        if ((err = cdev_add(&dev->doom_cdev, doom_major, 1)))
                goto err_cdev;

        dev->device = device_create(doom_class, 0, doom_major + dev->minor,
                                    0, "doom%d", dev->minor);
        if (IS_ERR(dev->device)) {
                err = PTR_ERR(dev->device);
                goto err_device;
        }

        return 0;
        err_device:
        cdev_del(&dev->doom_cdev);
        err_cdev:
        return err;
}

void platform_deregister(struct doom_device *dev, dev_t doom_major, struct class *doom_class)
{
        device_destroy(doom_class, doom_major + dev->minor);
        cdev_del(&dev->doom_cdev);
}
