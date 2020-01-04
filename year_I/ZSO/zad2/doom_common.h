#ifndef DOOM_COMMON_H
#define DOOM_COMMON_H

#include "debug.h"

#define CMD_MMIO_SIZE 512
#define HARDDOOM_COMMAND_SIZE 32
#define HARDDOOM_MAX_BUF_SIZE (1024 * 1024 * 4)

#if defined USE_CMD_BUF
#define MAX_CMD_BUFFER_SIZE (HARDDOOM_MAX_BUF_SIZE / HARDDOOM_COMMAND_SIZE)

#ifndef CMD_BUFFER_SIZE
        #define CMD_BUFFER_SIZE (1 << 17)
#endif

_Static_assert(CMD_BUFFER_SIZE <= MAX_CMD_BUFFER_SIZE, "Too big command buffer");

#define CMDS_QUEUE_SIZE CMD_BUFFER_SIZE
#define EFFECTIVE_QUEUE_SIZE (CMD_BUFFER_SIZE - 1)

#else /*!defined USE_CMD_BUF */

#define CMDS_QUEUE_SIZE CMD_MMIO_SIZE
#define EFFECTIVE_QUEUE_SIZE CMD_MMIO_SIZE

#endif


struct doom_setup {
        struct doom_surface *surf_dst;
        struct doom_surface *surf_src;
        struct doom_buffer *texture;
        struct doom_buffer *flat;
        struct doom_buffer *colormap;
        struct doom_buffer *translation;
        struct doom_buffer *tranmap;
};

#include "doom_context.h"
#include "doom_device.h"

#endif /* DOOM_COMMON_H */
