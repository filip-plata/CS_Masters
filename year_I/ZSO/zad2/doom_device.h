#ifndef DOOMDEVICE_H
#define DOOMDEVICE_H

#include <linux/fs.h>
#include <linux/file.h>
#include <linux/mutex.h>
#include <linux/wait.h>
#include <linux/list.h>
#include <linux/radix-tree.h>
#include <linux/cdev.h>
#include <asm/spinlock.h>

#include "doom_common.h"
#include "doomdev2.h"

struct doom_pending_batch {
        /* Last batch number this setup is required for.
         * Has to be initialized with -1. This is set to real
         * value when setup is changed, since only then we know
         * for how long it will be needed. */
        uint64_t batch_nr;
        struct list_head list;
        struct doom_setup setup;
};


struct doom_device {
        /* Three fields protected by driver lock. used has to be first */
        bool used;
        uint64_t open_count;
        uint8_t minor;

        struct pci_dev *pci_device;
        struct device *device;
        struct cdev doom_cdev;

        void __iomem *bar;

#ifdef USE_CMD_BUF
        /* buffer position cache to save one MMIO operation */
        struct doom_raw_buf *cmd_buf;
        uint64_t cmd_buffer_pos;
#endif

        struct mutex lock;
        /* Current setup of the device */
        struct doom_setup dev_setup;

        uint64_t cmds_count;
        bool pong_async_enabled;
        uint32_t queue_space_cache;
        uint32_t reserved_queue_space;
        uint32_t waiting;
        wait_queue_head_t device_queue;
        spinlock_t device_queue_spinlock;

        spinlock_t fence_lock;
        uint64_t overflows;
        uint64_t sended_batches;
        // fence counter cache only increases
        uint64_t fence_counter_cache;
        bool expecting_overflow;
        wait_queue_head_t fence_queue;
        struct list_head pending_batches;

        void *irq_data;
};

void doom_device_init(struct doom_device *);
void doom_dev_cleanup(struct doom_device **, struct doom_device *);

#ifdef USE_CMD_BUF
int doom_device_allocate_cmd_buffer(struct doom_device *);
void doom_device_free_cmd_buffer(struct doom_device *);
#endif

void doom_free_if_unused(struct doom_device **, struct doom_device *);
void set_device_setup(struct doom_device *, struct doom_setup *);

void perform_setup_bookkeeping(struct doom_device *, struct doom_pending_batch *);
void clear_all_batches(struct doom_device *);

#endif /* DOOMDEVICE_H */
