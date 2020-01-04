#include <linux/pci.h>

#include "doom_parse_cmd.h"
#include "harddoom_interface.h"
#include "doomdev2.h"
#include "doom_dma.h"
#include "doom_device.h"
#include "doomcode2.h"
#include "harddoom2.h"
#include "doom_irqs.h"

#ifdef USE_CMD_BUF
        #define HARDDOOM2_ENABLE_MASK HARDDOOM2_ENABLE_ALL
#else
        #define HARDDOOM2_ENABLE_MASK (HARDDOOM2_ENABLE_ALL & \
                ~(HARDDOOM2_ENABLE_CMD_FETCH))
#endif


int do_harddoom_init(struct doom_device *dev)
{
        int i;
        void __iomem *b = dev->bar;

        iowrite32(0, b + HARDDOOM2_FE_CODE_ADDR);

        for (i = 0; i < sizeof(doomcode2) / 4; i++)
                iowrite32(doomcode2[i], b + HARDDOOM2_FE_CODE_WINDOW);

        iowrite32(HARDDOOM2_RESET_ALL, b + HARDDOOM2_RESET);

#ifdef USE_CMD_BUF
        iowrite32(get_trans_table_dma_addr(dev->cmd_buf),
                b + HARDDOOM2_CMD_PT);
        iowrite32(get_buffer_size(dev->cmd_buf) / sizeof(struct cmd_format_all),
                b + HARDDOOM2_CMD_SIZE);

        iowrite32(0, b + HARDDOOM2_CMD_READ_IDX);
        iowrite32(0, b + HARDDOOM2_CMD_WRITE_IDX);
#endif

        reset_irqs(dev);
        enable_irqs(dev);

        iowrite32(dev->fence_counter_cache, b + HARDDOOM2_FENCE_COUNTER);
        iowrite32(HARDDOOM2_ENABLE_MASK, b + HARDDOOM2_ENABLE);

        return 0;
}

#ifdef USE_CMD_BUF

static void send_commands_to_buffer(
        struct doom_device *dev, struct cmd_format_all *cmds, size_t cmds_c)
{
        struct doom_raw_buf *buf = dev->cmd_buf;
        int64_t buf_pos = dev->cmd_buffer_pos,
               q_s = CMDS_QUEUE_SIZE,
               cmds_batch_size = cmds_c;

        if (cmds_c + buf_pos > q_s) {
                write_to_buf_kernel(
                buf,
                (char *) (cmds + (q_s - buf_pos)),
                (cmds_c - (q_s - buf_pos)) * sizeof(struct cmd_format_all),
                0);
                cmds_c = q_s - buf_pos;
        }

        write_to_buf_kernel(buf, (char *) cmds,
                            cmds_c * sizeof(struct cmd_format_all),
                            buf_pos * sizeof(struct cmd_format_all));
        dev->cmd_buffer_pos = (buf_pos + cmds_batch_size) % q_s;
        iowrite32(dev->cmd_buffer_pos, dev->bar + HARDDOOM2_CMD_WRITE_IDX);
}

#else

static long send_command_mmio(struct doom_device *dev, struct cmd_format_all *cmd)
{
        int i;
        uint32_t *data = (uint32_t *) cmd;

        for (i = 0; i < sizeof(struct cmd_format_all) / 4; i++)
                iowrite32(data[i], dev->bar + HARDDOOM2_CMD_SEND(i));

        return 0;
}

#endif

void do_device_commands(struct doom_device *dev,
                        struct cmd_format_all *cmds,
                        size_t cmds_c)
{
#ifdef USE_CMD_BUF
        send_commands_to_buffer(dev, cmds, cmds_c);
#else
        int i;
        for (i = 0; i < cmds_c; i++)
                send_command_mmio(dev, cmds + i);
#endif
}

int do_harddoom_stop(struct doom_device *dev)
{
        int res;
        disable_irqs(dev);
        iowrite32(0, dev->bar + HARDDOOM2_ENABLE);
        res = 0 == ioread32(dev->bar + HARDDOOM2_ENABLE);
        clear_all_batches(dev);
        return res;
}

uint64_t get_queue_free_place(struct doom_device *dev)
{
        uint64_t res;
        void __iomem *b = dev->bar;
        #ifdef USE_CMD_BUF
        res = (ioread32(b + HARDDOOM2_CMD_READ_IDX)
              - ioread32(b + HARDDOOM2_CMD_WRITE_IDX)
              + CMDS_QUEUE_SIZE - 1) % CMDS_QUEUE_SIZE;
        #else
        res = ioread32(b + HARDDOOM2_CMD_FREE);
        #endif
        return res;
}
