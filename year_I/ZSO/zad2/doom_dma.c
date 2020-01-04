#include <linux/kernel.h>
#include <linux/ioctl.h>
#include <linux/uaccess.h>
#include <linux/fs.h>
#include <linux/file.h>
#include <linux/cdev.h>
#include <linux/slab.h>
#include <linux/anon_inodes.h>
#include <linux/pci.h>
#include <linux/kref.h>

#include "doom_dma.h"
#include "doom_validate.h"
#include "doom_fence.h"

#define buffer_f_flags (FMODE_LSEEK | FMODE_PREAD | FMODE_PWRITE | \
                       FMODE_WRITE | FMODE_READ)
#define TR_TABLE_MAX_SIZE (1 << 12)
#define TR_TABLE_ALINGMENT (1 << 8)
#define DOOM_PAGE_SIZE (1 << 12)

static char doom_srf[] = "[doom-surface]";
static char doom_buf[] = "[doom-buffer]";
static char doom_command_buf[] = "[doom-command-buffer]";

struct harddoom_addr {
        bool valid : 1;
        bool writable : 1;
        char padding : 2;
        uint32_t addr : 28;
};

_Static_assert (sizeof(struct harddoom_addr) == 4,
                "Comand harddom addr length");

struct doom_page {
        dma_addr_t dma_addr;
        void *v_addr;
};

struct doom_contingous_range {
        void *start;
        size_t size;
};

enum doom_raw_buffer_type {
	DOOM_SURFACE = 0,
	DOOM_BUFFER = 1,
        DOOM_CMD = 2,
};

struct doom_raw_buf {
        uint32_t size;
        struct device dev;
        struct doom_device *doom_device;

        int32_t fd;
        struct file *s_f;

        struct doom_page trans;
        struct doom_page *pages;
        uint32_t pages_c;

        struct kref refcount;

        enum doom_raw_buffer_type type;
        void *parent;

        int64_t pending_command_batch;
        struct mutex buf_lock;
};


static inline size_t pages_count(size_t size)
{
        // adding here +1 protects from strange page fault on doom
        // I was unable to debug it
        return (size + DOOM_PAGE_SIZE - 1) / DOOM_PAGE_SIZE + 1;
}

/*
* Finds largest contingous range in buffer which starts
* at start and is does not cross end offset
*/
static struct doom_contingous_range largest_cont_range(
        struct doom_raw_buf *buf, loff_t start, loff_t end)
{
        struct doom_contingous_range res;
        loff_t page_off = (start >> 12) << 12;
        loff_t page_idx = page_off / DOOM_PAGE_SIZE;
        loff_t page_end = page_off + DOOM_PAGE_SIZE;
        loff_t in_page_off = start - page_off;

        res.start = buf->pages[page_idx].v_addr + in_page_off;
        res.size = min(end, page_end) - start;

        return res;
}

int generic_process_range(
        struct doom_raw_buf *t_data, char *buf, size_t count,
        size_t pos, int (*process)(void *v, struct doom_contingous_range *r))
{
        int err;
        size_t processed = 0, end = pos + count;
        struct doom_contingous_range curr_range;

        while (processed < count) {
                curr_range = largest_cont_range(t_data, pos + processed, end);
                if ((err = process(buf + processed, &curr_range)))
                        return err;

                processed += curr_range.size;
        }

        return processed;
}

int process_memcpy(void *v, struct doom_contingous_range *r)
{
        memcpy(r->start, v, r->size);
        return 0;
}

void write_to_buf_kernel(struct doom_raw_buf *t_data, char *buf, size_t count, size_t pos)
{
        generic_process_range(t_data, buf, count, pos, &process_memcpy);
}

int process_copy_to_user(void *v, struct doom_contingous_range *r)
{
        if (copy_to_user(v, r->start, r->size))
                return -EFAULT;
        return 0;
}

static ssize_t do_buffer_read(struct doom_raw_buf *t_data, char __user *buf, size_t count, size_t pos)
{
        return generic_process_range(t_data, buf, count, pos,
                &process_copy_to_user);
}

static ssize_t perform_buffer_operation(
        struct doom_raw_buf *t_data, char __user *buf, size_t count, size_t pos,
        ssize_t (*operation)(struct doom_raw_buf *, char __user *, size_t, size_t))
{
        long err;
        volatile int64_t fence_w = t_data->pending_command_batch;

        if (unlikely(!t_data->doom_device->used))
                return -EIO;

        if (count > t_data->size - pos)
                count = t_data->size - pos;

        if (fence_w != -1) {
                // this does not wait if operation was already done
                if ((err = fence_await(t_data->doom_device, fence_w)))
                        goto err_wait_for_commands;
        }

        if ((err = mutex_lock_interruptible(&t_data->buf_lock)))
                goto err_mutex_lock;

        err = operation(t_data, buf, count, pos);

        mutex_unlock(&t_data->buf_lock);
        err_mutex_lock:
        err_wait_for_commands:
        return err;
}

static ssize_t buffer_read(struct file *file, char __user *buf, size_t count, loff_t *filepos)
{
        long err;
        loff_t pos = *filepos;
        struct doom_raw_buf *t_data = file->private_data;

        if (pos > t_data->size || pos < 0)
                return 0;

        err = perform_buffer_operation(t_data, buf, count, pos, &do_buffer_read);

        if (!IS_ERR_VALUE(err))
                *filepos = pos + count;
        return err;
}

int process_copy_from_user(void *v, struct doom_contingous_range *r)
{
        if (copy_from_user(r->start, v, r->size))
                return -EFAULT;
        return 0;
}

static ssize_t do_buffer_write(struct doom_raw_buf *t_data, char __user *buf, size_t count, size_t pos)
{
        return generic_process_range(t_data, (void *) buf, count, pos,
                &process_copy_from_user);
}

static ssize_t buffer_write(struct file *file, const char __user *bufer, size_t count, loff_t *filepos)
{
        long err;
        loff_t pos = *filepos;
        struct doom_raw_buf *t_data = file->private_data;

        if (pos > t_data->size || pos < 0)
                return -ENOSPC;

        err = perform_buffer_operation(t_data, (char __user *) bufer,
                count, pos, &do_buffer_write);

        if (!IS_ERR_VALUE(err))
                *filepos = pos + err;
        return err;
}

static void free_buf(struct doom_raw_buf *buf)
{
        int i;

        for (i = 0; i < buf->pages_c; i++)
                dma_free_coherent(&buf->dev, DOOM_PAGE_SIZE,
                        buf->pages[i].v_addr, buf->pages[i].dma_addr);
        kfree(buf->pages);

        if (buf->parent)
                kfree(buf->parent);
        kfree(buf);
}

static void free_buf_kref(struct kref *kref)
{
        free_buf(container_of(kref, struct doom_raw_buf, refcount));
}

static int doom_buffer_release(struct inode *ino, struct file *filep)
{
        struct doom_raw_buf *buf = filep->private_data;
        put_doom_buf(buf);
        return 0;
}

static struct file_operations buffer_fops = {
        .owner = THIS_MODULE,
        .read = buffer_read,
        .write = buffer_write,
        .release = doom_buffer_release,
};

static inline int is_doom_buf_file(struct file *file)
{
        return file->f_op == &buffer_fops;
}

static void build_translation_table(struct doom_raw_buf *buf, bool writable)
{
        int i;
        size_t pages = pages_count(buf->size);
        struct harddoom_addr *addr = buf->trans.v_addr;

        WARN_ON(buf->size > (1 << 22));
        WARN_ON((uint64_t) buf->trans.dma_addr % TR_TABLE_ALINGMENT);

        for (i = 0; i < pages; i++) {
                *addr = (struct harddoom_addr) {
                        .valid=true,
                        .writable = writable,
                        .padding = '\0',
                        .addr=((uint32_t) (buf->pages[i].dma_addr >> 12))
                };
                addr++;
        }
}

static struct doom_raw_buf *create_doom_buf_basic(size_t size)
{
        struct doom_raw_buf *buf = kmalloc(sizeof(struct doom_raw_buf), GFP_KERNEL);

        if (IS_ERR_OR_NULL(buf))
                return buf;
        memset(buf, '\0', sizeof(struct doom_raw_buf));
        kref_init(&buf->refcount);
        mutex_init(&buf->buf_lock);

        buf->size = size;
        return buf;
}

static struct doom_page create_doom_page(struct doom_raw_buf *buf, size_t size)
{
        struct doom_page page;

        page.v_addr = dma_alloc_coherent(&buf->dev, size,
                                         &page.dma_addr, GFP_KERNEL);
        BUG_ON(page.dma_addr % DOOM_PAGE_SIZE != 0);
        return page;
}

static int allocate_file(struct doom_raw_buf *buf, const char *buffer_name)
{
        buf->s_f = anon_inode_getfile(buffer_name, &buffer_fops, buf, O_RDWR);

        if (IS_ERR(buf->s_f))
                return PTR_ERR(buf->s_f);
        buf->s_f->f_mode |= buffer_f_flags;
        return 0;
}

/* It has to be the last method called */
static int allocate_descriptor(struct doom_raw_buf *buf)
{
        long err, fd;

        err = get_unused_fd_flags(O_RDWR);
        if (IS_ERR_VALUE(err))
                return err;
        fd = err;
        buf->fd = fd;

        fd_install(fd, buf->s_f);
        return 0;
}

static int allocate_dma_pages(struct doom_raw_buf *buf, bool writable)
{
        int i;
        size_t page_off = buf->size % DOOM_PAGE_SIZE;
        size_t pages = pages_count(buf->size);
        size_t tr_table_size = pages * sizeof(struct harddoom_addr);
        size_t tr_table_offset = round_up(page_off + 1, TR_TABLE_ALINGMENT);
        bool separate_alloc = (page_off == 0  || page_off > (DOOM_PAGE_SIZE - 3)) ||
                ((DOOM_PAGE_SIZE - tr_table_offset) < tr_table_size);
        struct doom_page *last_page;

        if (separate_alloc)
                pages++;

        buf->pages_c = pages;
        buf->pages = kmalloc(sizeof(struct doom_page) * pages, GFP_KERNEL);
        if (IS_ERR_OR_NULL(buf->pages))
                return PTR_ERR(buf->pages);

        for (i = 0; i < pages; i++)
                buf->pages[i] = create_doom_page(buf, DOOM_PAGE_SIZE);
        last_page = buf->pages + pages - 1;

        if (separate_alloc) {
                buf->trans = *last_page;
        }
        else {
                buf->trans.dma_addr = last_page->dma_addr + tr_table_offset;
                buf->trans.v_addr = last_page->v_addr + tr_table_offset;
        }

        build_translation_table(buf, writable);
        return 0;
}

static struct doom_raw_buf * setup_raw_buffer(
        const char *buffer_name,
        struct doom_device *dev,
        size_t size,
        bool writable,
        bool alloc_fd)
{
        int err;
        struct doom_raw_buf *buf = create_doom_buf_basic(size);

        if (IS_ERR_OR_NULL(buf)) {
                err = -ENOMEM;
                goto err_no_mem_meta;
        }

        buf->dev = dev->pci_device->dev;
        buf->doom_device = dev;
        buf->pending_command_batch = -1;

        if ((err = allocate_dma_pages(buf, writable)))
                goto err_allocate_dma;

        if (alloc_fd) {
                if ((err = allocate_file(buf, buffer_name)))
                        goto err_alocate_fd;
        }

        return buf;

        err_alocate_fd:
        err_allocate_dma:
        free_buf(buf);
        err_no_mem_meta:
        printk(KERN_ERR "Error when creating harddoom buffer");
        return ERR_PTR(err);
}

struct doom_surface * create_surface(struct doom_device *dev,
                    struct doomdev2_ioctl_create_surface surface)
{
        int err;
        struct doom_raw_buf *buf_raw;
        struct doom_surface *srf;

        if ((err = validate_create_surface(&surface)))
            return ERR_PTR(err);

        srf = kmalloc(sizeof(struct doom_surface), GFP_KERNEL);
        if (IS_ERR_OR_NULL(srf)) {
                err = PTR_ERR(srf);
                goto err_kmalloc_surface;
        }

        buf_raw = setup_raw_buffer(doom_srf,
                dev, surface.width * surface.height, true, true);

        if (IS_ERR(buf_raw)) {
                err = PTR_ERR(buf_raw);
                goto err_setup_buffer;
        }

        srf->buf = buf_raw;
        srf->width = surface.width;
        srf->height = surface.height;
        buf_raw->type = DOOM_SURFACE;
        buf_raw->parent = srf;

        if ((err = allocate_descriptor(buf_raw)))
                goto err_alloc_descriptor;

        return srf;
        err_alloc_descriptor:
        free_buf(buf_raw);
        err_setup_buffer:
        kfree(srf);
        err_kmalloc_surface:
        return ERR_PTR(err);
}

struct doom_buffer * create_buffer(struct doom_device *dev,
                             struct doomdev2_ioctl_create_buffer buffer)
{
        int err;
        struct doom_raw_buf *buf_raw;
        struct doom_buffer *buf;

        if ((err = validate_create_buffer(&buffer)))
                return ERR_PTR(err);

        buf = kmalloc(sizeof(struct doom_buffer), GFP_KERNEL);
        if (IS_ERR_OR_NULL(buf)) {
                err = PTR_ERR(buf);
                goto err_kmalloc_buffer;
        }

        buf_raw = setup_raw_buffer(doom_buf, dev, buffer.size, false, true);

        if (IS_ERR(buf_raw)) {
                err = PTR_ERR(buf_raw);
                goto err_setup_buffer;
        }

        buf->buf = buf_raw;
        buf_raw->type = DOOM_BUFFER;
        buf_raw->parent = buf;

        if ((err = allocate_descriptor(buf_raw)))
                goto err_alloc_descriptor;

        return buf;
        err_alloc_descriptor:
        free_buf(buf_raw);
        err_setup_buffer:
        kfree(buf);
        err_kmalloc_buffer:
        return ERR_PTR(err);
}

struct doom_raw_buf *create_command_buffer(struct doom_device *device, size_t size)
{
        struct doom_raw_buf *buf_raw;

        buf_raw = setup_raw_buffer(doom_command_buf, device, size, false, false);

        if (IS_ERR(buf_raw))
                goto err_setup_buffer;

        buf_raw->type = DOOM_CMD;

        err_setup_buffer:
        return buf_raw;
}

static struct doom_raw_buf * get_doom_raw_buf(int32_t fd)
{
        struct file *file;
        struct doom_raw_buf *res;

        if (fd == -1)
                return NULL;
        if (fd < 0)
                return ERR_PTR(-EBADF);
        file = fget(fd);

        if (IS_ERR(file))
                return ERR_PTR(-EBADF);

        if (!is_doom_buf_file(file))
                return ERR_PTR(-EINVAL);

        res = file->private_data;
        /* Decreasing file usage count after bumping kref.
         * Driver should not increase file usage count. */
        get_doom_buf(res);
        fput(file);

        return res;
}

static void *get_doom_buf_class(struct doom_device *dev, int32_t fd,
        enum doom_raw_buffer_type type)
{

        struct doom_raw_buf *raw_buf = get_doom_raw_buf(fd);

        if (IS_ERR_OR_NULL(raw_buf))
                return raw_buf;

        if (raw_buf->type != type || raw_buf->doom_device != dev)
                return ERR_PTR(-EBADF);

        return raw_buf->parent;
}

struct doom_buffer *get_doom_buffer(struct doom_device *dev, int32_t fd)
{
        return get_doom_buf_class(dev, fd, DOOM_BUFFER);
}

struct doom_surface *get_doom_surface(struct doom_device *dev, int32_t fd)
{
        return get_doom_buf_class(dev, fd, DOOM_SURFACE);
}

void put_doom_buf(struct doom_raw_buf *buf)
{
        kref_put(&buf->refcount, &free_buf_kref);
}

void get_doom_buf(struct doom_raw_buf *buf)
{
        kref_get(&buf->refcount);
}

uint32_t get_trans_table_dma_addr(struct doom_raw_buf *buf)
{
        return (uint32_t) (buf->trans.dma_addr >> 8);
}

int get_doom_buf_fd(struct doom_raw_buf *buf) {
        return buf->fd;
}

size_t get_buffer_size(struct doom_raw_buf *buf)
{
        return buf->size;
}

void set_pending_batch_number(struct doom_raw_buf *buf, int64_t batch_nr)
{
        buf->pending_command_batch = batch_nr;
}

int64_t get_pending_batch_number(struct doom_raw_buf *buf)
{
        return buf->pending_command_batch;
}
