#ifndef DOOM_BUFFER_H
#define DOOM_BUFFER_H

/* Module providing interface to doom buffers */
#include <linux/types.h>

#include "doom_common.h"

struct doom_buffer {
        // Raw buf pointer has to be first member
        struct doom_raw_buf *buf;
};

struct doom_surface {
        // Raw buf pointer has to be first member
        struct doom_raw_buf *buf;
        uint16_t width;
        uint16_t height;
};

/* Create a surface on a device.
 * Adds them to buffers on device context */
struct doom_surface * create_surface(struct doom_device *,
                    struct doomdev2_ioctl_create_surface);
/* Creates plain buffer on the device.
 * Adds them to buffers on device context */
struct doom_buffer * create_buffer(struct doom_device *,
                             struct doomdev2_ioctl_create_buffer);
struct doom_raw_buf *create_command_buffer(struct doom_device *, size_t);
/* Finds a buffer given file descriptor */
struct doom_buffer * get_doom_buffer(struct doom_device *, int32_t fd);
struct doom_surface * get_doom_surface(struct doom_device *, int32_t fd);

void put_doom_buf(struct doom_raw_buf *);
void get_doom_buf(struct doom_raw_buf *);

uint32_t get_trans_table_dma_addr(struct doom_raw_buf *);
int get_doom_buf_fd(struct doom_raw_buf *);

size_t get_buffer_size(struct doom_raw_buf *);

void write_to_buf_kernel(struct doom_raw_buf *, char *, size_t, size_t);

void set_pending_batch_number(struct doom_raw_buf *, int64_t);
int64_t get_pending_batch_number(struct doom_raw_buf *);

#endif /* DOOM_BUFFER_H */
