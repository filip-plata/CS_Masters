#include <linux/module.h>
#include <linux/file.h>
#include <linux/pci.h>

MODULE_LICENSE("GPL");

#include "doom_device.h"
#include "doom_dma.h"
#include "harddoom_interface.h"
#include "doom_common.h"
#include "doom_reg.h"
#include "doom_device.h"
#include "doom_engine.h"
#include "doom_fence.h"
#include "doom_context.h"
#include "debug.h"

#define MAX_DOOM_DEVICES 256
#define HARDDOOM_VENDOR 0x0666
#define HARDDOOM_DEV_NR 0x1994


const char driver_name[] = "harddoom-driver";
DEFINE_MUTEX(driver_lock);

static struct doom_device *dooms[MAX_DOOM_DEVICES];

static dev_t doom_major;
static struct class doom_class = {
        .name = "doom",
        .owner = THIS_MODULE,
};


static int doom_open(struct inode *ino, struct file *filep)
{
        int err = 0;
        dev_t minor = MINOR(ino->i_rdev);
        struct doom_device *dev;
        struct doom_context *ctx;

        if (!(0 <= minor && minor < MAX_DOOM_DEVICES))
                return -EINVAL;
        dev = dooms[minor];

        ctx = doom_context_create();
        if (IS_ERR(ctx)) {
                err = PTR_ERR(ctx);
                goto err_doom_context_create;
        }

        if ((err = mutex_lock_interruptible(&driver_lock)))
                goto err_driver_lock;

        if (unlikely(!dev->used)) {
                err = -EIO;
                goto err_device_gone;
        }
        dev->open_count++;
        ctx->dev = dev;
        filep->private_data = ctx;

        err_device_gone:
        mutex_unlock(&driver_lock);
        err_driver_lock:
        if (err)
                put_doom_context(ctx);
        err_doom_context_create:
        return err;
}

static int doom_release(struct inode *ino, struct file *filep)
{
        int err;
        struct doom_context *ctx = filep->private_data;
        struct doom_device *dev = ctx->dev;

        if ((err = mutex_lock_interruptible(&driver_lock)))
                goto err_driver_lock;

        ctx->dev->open_count--;
        doom_free_if_unused(dooms, dev);
        put_doom_context(ctx);

        mutex_unlock(&driver_lock);
        err_driver_lock:
        return err;
}

static struct file_operations doom_fops = {
        .owner = THIS_MODULE,
        .write = doom_write,
        .open = doom_open,
        .unlocked_ioctl = doom_ioctl,
        .compat_ioctl = doom_ioctl,
        .release = doom_release,
};

static struct pci_device_id harddoom_id = {
        PCI_DEVICE(HARDDOOM_VENDOR, HARDDOOM_DEV_NR)
};

static struct doom_device *get_unused_minor(void)
{
        static uint8_t idx = 0;
        uint8_t curr_idx;
        struct doom_device *res = NULL;
        long err;

        if ((err = mutex_lock_interruptible(&driver_lock))) {
                res = ERR_PTR(err);
                goto err_mutex_lock;
        }
        curr_idx = idx;

        do {
                if (!dooms[curr_idx]) {
                        res = kmalloc(sizeof(struct doom_device),
                                GFP_KERNEL);
                        if (IS_ERR_OR_NULL(res)) {
                                res = ERR_PTR(-ENOMEM);
                                goto err_alloc_doom_device;
                        }
                        dooms[curr_idx] = res;
                        break;
                }
                curr_idx++;
        } while (curr_idx != idx);

        idx = curr_idx;
        err_alloc_doom_device:
        mutex_unlock(&driver_lock);

        if (!IS_ERR_OR_NULL(res)) {
                doom_device_init(res);
                res->minor = curr_idx;
        }
        /* If no place in global table was found, return error */
        if (res == NULL)
                res = ERR_PTR(-ENOSPC);
        err_mutex_lock:
        return res;
}

static int doom_dev_setup(struct pci_dev *pci_dev, const struct pci_device_id *id)
{
        int err;
        struct doom_device *dev = get_unused_minor();

        if (IS_ERR_OR_NULL(dev)) {
                printk(KERN_ERR "Unable to support more than %d devices",
                        MAX_DOOM_DEVICES);
                err = PTR_ERR(dev);
                goto err_basic_setup;
        }

        if ((err = pci_register_doom_device(pci_dev, dev)))
                goto err_pci;

        if ((err = platform_register(dev, doom_major, &doom_fops, &doom_class)))
                goto err_platform;

#ifdef USE_CMD_BUF
        if ((err = doom_device_allocate_cmd_buffer(dev)))
                goto err_allocate_cmd_buf;
#endif

        if ((err = do_harddoom_init(dev)))
            goto err_harddoom_init;

        return 0;

        err_harddoom_init:
#ifdef USE_CMD_BUF
        err_allocate_cmd_buf:
#endif
        platform_deregister(dev, doom_major, &doom_class);
        err_platform:
        pci_deregister_doom_device(dev);
        err_pci:
        doom_dev_cleanup(dooms, dev);
        err_basic_setup:
        return err;
}

static void doom_dev_remove(struct pci_dev *pci_dev)
{
        struct doom_device *dev =
                (struct doom_device *) pci_get_drvdata(pci_dev);

        mutex_lock(&driver_lock);
        mutex_lock(&dev->lock);
        dev->used = false;
#ifdef USE_CMD_BUF
        doom_device_free_cmd_buffer(dev);
#endif
        mutex_unlock(&dev->lock);

        do_harddoom_stop(dev);
        platform_deregister(dev, doom_major, &doom_class);

        pci_deregister_doom_device(dev);

        doom_dev_cleanup(dooms, dev);
        mutex_unlock(&driver_lock);
}


static int doom_suspend(struct pci_dev *pci_dev, pm_message_t state)
{
        struct doom_device *dev = pci_get_drvdata(pci_dev);
        mutex_lock(&dev->lock);
        dev->used = false;
        fence_await(dev, dev->sended_batches);
        do_harddoom_stop(dev);
        mutex_unlock(&dev->lock);
        return 0;
}

static int doom_resume(struct pci_dev *pci_dev)
{
        struct doom_device *dev = pci_get_drvdata(pci_dev);
        mutex_lock(&dev->lock);
        do_harddoom_init(dev);
        mutex_unlock(&dev->lock);
        return 0;
}

static struct pci_driver doom_driver = {
        .name = "doom-driver",
        .id_table = &harddoom_id,
        .probe = doom_dev_setup,
        .remove = doom_dev_remove,
        .suspend = doom_suspend,
        .resume = doom_resume,
};

static __init int doom_init(void)
{
        int err;
        memset(dooms, '\0', MAX_DOOM_DEVICES * sizeof(struct doom_device*));

        if ((err = alloc_chrdev_region(
                &doom_major, 0, MAX_DOOM_DEVICES, driver_name)))
                goto err_alloc;
        if ((err = class_register(&doom_class)))
                goto err_class;
        if ((err = pci_register_driver(&doom_driver)))
                goto err_pci;

        return 0;

        err_pci:
        class_unregister(&doom_class);
        err_class:
        unregister_chrdev_region(doom_major, MAX_DOOM_DEVICES);
        err_alloc:
        printk(KERN_ERR "Unable to initialize harddoom driver");
        return err;
}

static __exit void doom_cleanup(void)
{
        pci_unregister_driver(&doom_driver);
        class_unregister(&doom_class);
        unregister_chrdev_region(doom_major, MAX_DOOM_DEVICES);
}

module_init(doom_init);
module_exit(doom_cleanup);
