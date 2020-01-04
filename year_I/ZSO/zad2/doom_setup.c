#include "doom_common.h"
#include "doom_dma.h"

static int is_valid_buffer(struct doom_raw_buf **buf)
{
        return !IS_ERR_OR_NULL(buf);
}

static void get_doom_buf_if_valid(struct doom_raw_buf **buf)
{
        if (is_valid_buffer(buf))
                get_doom_buf(*buf);
}

void get_doom_setup(struct doom_setup *setup)
{
        int i;
        // This cast is a bit of hack. But it works
        struct doom_raw_buf ***bufs = (struct doom_raw_buf ***) setup;
        for (i = 0; i < 7; i++)
                get_doom_buf_if_valid(bufs[i]);
}

static void put_doom_buf_if_valid(struct doom_raw_buf **buf)
{
        if (is_valid_buffer(buf))
                put_doom_buf(*buf);
}

void put_doom_setup(struct doom_setup *setup)
{
        int i;
        struct doom_raw_buf ***bufs = (struct doom_raw_buf ***) setup;
        for (i = 0; i < 7; i++)
                put_doom_buf_if_valid(bufs[i]);
}
