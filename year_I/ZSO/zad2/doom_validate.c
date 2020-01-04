#include <linux/err.h>

#include "doom_validate.h"
#include "doom_dma.h"

#define OK 0

#define BUFFER_MAX_SIZE (1 << 22)

#define SURFACE_MIN_HEIGHT 1
#define SURFACE_MAX_HEIGHT 2048
#define SURFACE_WIDTH_MOD 64
#define SURFACE_MIN_WIDTH SURFACE_WIDTH_MOD
#define SURFACE_MAX_WIDTH 2048
#define FUZZ_POS_MAX 55
#define TEXTURE_SIZE (64 * 64)
#define COLORMAP_SIZE 256
#define TRANMAP_SIZE (256 * 256)
#define TRANSLATION_SIZE 256
#define FLAT_SIZE (1 << 12)


struct rect {
        uint32_t x_a;
        uint32_t y_a;
        uint32_t x_b;
        uint32_t y_b;
};

int validate_setup_cmd(struct doom_setup *setup)
{
        if (IS_ERR(setup->surf_dst))
                return PTR_ERR(setup->surf_dst);
        if (IS_ERR(setup->surf_src))
                return PTR_ERR(setup->surf_src);
        if (IS_ERR(setup->texture))
                return PTR_ERR(setup->texture);
        if (IS_ERR(setup->flat))
                return PTR_ERR(setup->flat);
        if (IS_ERR(setup->colormap))
                return PTR_ERR(setup->colormap);
        if (IS_ERR(setup->translation))
                return PTR_ERR(setup->translation);
        if (IS_ERR(setup->tranmap))
                return PTR_ERR(setup->tranmap);


        if (setup->flat != NULL &&
           get_buffer_size(setup->flat->buf) % TEXTURE_SIZE != 0)
                return -EINVAL;

        if (setup->colormap != NULL &&
           get_buffer_size(setup->colormap->buf) % COLORMAP_SIZE != 0)
                return -EINVAL;

        if (setup->tranmap != NULL &&
            get_buffer_size(setup->tranmap->buf) != TRANMAP_SIZE)
                return -EINVAL;

        return OK;
}

int validate_create_surface(struct doomdev2_ioctl_create_surface *surface)
{
        if (surface->width < SURFACE_MIN_WIDTH ||
            surface->width > SURFACE_MAX_WIDTH ||
            surface->width % SURFACE_WIDTH_MOD != 0 ||
            surface->height > SURFACE_MAX_HEIGHT ||
            surface->height < SURFACE_MIN_HEIGHT)
            return -EOVERFLOW;
        return OK;
}

int validate_create_buffer(struct doomdev2_ioctl_create_buffer *buffer)
{
        if (buffer->size > BUFFER_MAX_SIZE)
                return -EOVERFLOW;
        return OK;
}

static int validate_rectangle_in_buf(struct doom_surface *buf, struct rect r)
{
        /* Expects non NULL input */
        uint32_t w = buf->width;
        uint32_t h = buf->height;

        if (r.x_a > w ||
            r.y_a > h ||
            r.x_b > w ||
            r.y_b > h)
                return -EINVAL;

        return OK;
}

static int validate_idx_in_buffer(
        struct doom_raw_buf *buf, size_t idx, size_t step)
{
        if (get_buffer_size(buf) < (idx + 1) * step)
                return -EINVAL;
        return OK;
}

static int validate_cmd_copy_rect(struct doom_setup *setup,
                                  struct doomdev2_cmd_copy_rect *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        struct doom_surface *src = setup->surf_src;
        uint16_t w = cmd->width;
        uint16_t h = cmd->height;

        if (src == NULL)
                return -EINVAL;

        /* overlap of rectangle */
        if (srf == src &&
            !(cmd->pos_src_y >= cmd->pos_dst_y + h ||
            cmd->pos_src_y + h <= cmd->pos_dst_y ||
            cmd->pos_src_x <= cmd->pos_dst_x + w ||
            cmd->pos_src_x + w >= cmd->pos_dst_x)) {
                return -EINVAL;
        }

        if (validate_rectangle_in_buf(srf, (struct rect)
                {.x_a = cmd->pos_src_x, .x_b = cmd->pos_src_x + w,
                 .y_a = cmd->pos_src_y, .y_b = cmd->pos_src_y + h}))
                 return -EINVAL;

        if (validate_rectangle_in_buf(src, (struct rect)
                 {.x_a = cmd->pos_dst_x, .x_b = cmd->pos_dst_x + w,
                  .y_a = cmd->pos_dst_y, .y_b = cmd->pos_dst_y + h}))
                  return -EINVAL;

        return OK;
}

static int validate_cmd_fill_rect(struct doom_setup *setup,
                                  struct doomdev2_cmd_fill_rect *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        uint16_t w = cmd->width;
        uint16_t h = cmd->height;

        if (cmd->pos_y + h > srf->height ||
            cmd->pos_x + w > srf->width)
                return -EINVAL;

        return OK;
}

static int validate_cmd_draw_line(struct doom_setup *setup,
                                  struct doomdev2_cmd_draw_line *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        size_t srf_size;

        srf_size = get_buffer_size(srf->buf);

        if (cmd->pos_a_x + cmd->pos_a_y * srf->width > srf_size)
                return -EINVAL;

        if (cmd->pos_b_x + cmd->pos_b_y  * srf->width > srf_size)
                return -EINVAL;

        return OK;
}

static int validate_cmd_draw_background(struct doom_setup *setup,
                                        struct doomdev2_cmd_draw_background *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        struct doom_buffer *flat = setup->flat;
        uint16_t w = cmd->width;
        uint16_t h = cmd->height;

        if (flat == NULL)
                return -EINVAL;

        if (validate_rectangle_in_buf(srf, (struct rect) {
                .x_a = cmd->pos_x, .y_a = cmd->pos_y,
                .x_b = cmd->pos_x + w, .y_b = cmd->pos_y + h
        }))
                return -EINVAL;

        if (validate_idx_in_buffer(flat->buf, cmd->flat_idx, FLAT_SIZE))
                return -EINVAL;

        return OK;
}

static int validate_cmd_draw_column(struct doom_setup *setup,
                                    struct doomdev2_cmd_draw_column *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        struct doom_buffer *clr = setup->colormap,
                           *trans = setup->translation;

        if (validate_rectangle_in_buf(srf, (struct rect) {
                .x_a = cmd->pos_x, .y_a = cmd->pos_a_y,
                .x_b = cmd->pos_x, .y_b = cmd->pos_b_y
        }))
                return -EINVAL;

        if ((cmd->flags & DOOMDEV2_CMD_FLAGS_TRANSLATE) && (trans == NULL ||
            validate_idx_in_buffer(trans->buf, cmd->translation_idx, TRANSLATION_SIZE)))
                return -EINVAL;
        if ((cmd->flags & DOOMDEV2_CMD_FLAGS_COLORMAP) && (clr == NULL ||
            validate_idx_in_buffer(clr->buf, cmd->colormap_idx, COLORMAP_SIZE)))
                return -EINVAL;
        return OK;
}

static int validate_cmd_draw_fuzz(struct doom_setup *setup,
                                  struct doomdev2_cmd_draw_fuzz *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        struct doom_buffer *cmap = setup->colormap;

        if (cmap == NULL)
                return -EINVAL;

        if (validate_rectangle_in_buf(srf, (struct rect) {
                .x_a = cmd->pos_x, .y_a = cmd->pos_a_y,
                .x_b = cmd->pos_x, .y_b = cmd->pos_b_y
        }))
                return -EINVAL;

        if (validate_idx_in_buffer(cmap->buf, cmd->colormap_idx, COLORMAP_SIZE))
                return -EINVAL;

        if (cmd->fuzz_pos > FUZZ_POS_MAX)
                return -EINVAL;

        if (cmd->pos_a_y < cmd->fuzz_start)
                return -EINVAL;

        if (cmd->pos_b_y > cmd->fuzz_end)
                return -EINVAL;

        if (cmd->pos_a_y > cmd->pos_b_y)
                return -EINVAL;

        if (cmd->fuzz_end >= srf->height)
                return -EINVAL;

        return OK;
}

static int validate_cmd_draw_span(struct doom_setup *setup,
                                  struct doomdev2_cmd_draw_span *cmd)
{
        struct doom_surface *srf = setup->surf_dst;
        struct doom_buffer *clr = setup->colormap,
                           *trans = setup->translation,
                           *tranmap = setup->tranmap,
                           *flat = setup->flat;

        if (flat == NULL)
                return -EINVAL;

        if (cmd->pos_a_x > cmd->pos_b_x)
                return -EINVAL;

        if (validate_rectangle_in_buf(srf, (struct rect) {
                .x_a = cmd->pos_a_x, .y_a = cmd->pos_y,
                .x_b = cmd->pos_b_x, .y_b = cmd->pos_y
        }))
                return -EINVAL;

        if (validate_idx_in_buffer(flat->buf, cmd->flat_idx, FLAT_SIZE))
                return -EINVAL;

        if ((cmd->flags & DOOMDEV2_CMD_FLAGS_TRANSLATE) && (trans == NULL ||
            validate_idx_in_buffer(trans->buf, cmd->translation_idx, TRANSLATION_SIZE)))
                return -EINVAL;
        if ((cmd->flags & DOOMDEV2_CMD_FLAGS_COLORMAP) && (clr == NULL ||
            validate_idx_in_buffer(clr->buf, cmd->colormap_idx, COLORMAP_SIZE)))
                return -EINVAL;
        if ((cmd->flags & DOOMDEV2_CMD_FLAGS_TRANMAP) && (tranmap == NULL))
                return -EINVAL;
        return OK;
}

static int validate_cmd(struct doom_setup *setup, struct doomdev2_cmd *cmd)
{
        switch (cmd->type) {
        case DOOMDEV2_CMD_TYPE_COPY_RECT:
                return validate_cmd_copy_rect(
                        setup, (struct doomdev2_cmd_copy_rect *) cmd);
        case DOOMDEV2_CMD_TYPE_FILL_RECT:
                return validate_cmd_fill_rect(
                        setup, (struct doomdev2_cmd_fill_rect *) cmd);
        case DOOMDEV2_CMD_TYPE_DRAW_LINE:
                return validate_cmd_draw_line(
                        setup, (struct doomdev2_cmd_draw_line *) cmd);
        case DOOMDEV2_CMD_TYPE_DRAW_BACKGROUND:
                return validate_cmd_draw_background(
                        setup, (struct doomdev2_cmd_draw_background *) cmd);
        case DOOMDEV2_CMD_TYPE_DRAW_COLUMN:
                return validate_cmd_draw_column(
                        setup, (struct doomdev2_cmd_draw_column *) cmd);
        case DOOMDEV2_CMD_TYPE_DRAW_FUZZ:
                return validate_cmd_draw_fuzz(
                        setup, (struct doomdev2_cmd_draw_fuzz *) cmd);
        case DOOMDEV2_CMD_TYPE_DRAW_SPAN:
                return validate_cmd_draw_span(
                        setup, (struct doomdev2_cmd_draw_span *) cmd);
        default:
                return -EINVAL;
        }
}

int validate_cmds_batch(struct doom_setup *setup, struct doomdev2_cmd *cmds,
                size_t size)
{
        int i, err;

        if (setup->surf_dst == NULL)
                return -EINVAL;

        for (i = 0; i < size; i++) {
                if ((err = validate_cmd(setup, cmds + i)))
                        return err;
        }

        return OK;
}
