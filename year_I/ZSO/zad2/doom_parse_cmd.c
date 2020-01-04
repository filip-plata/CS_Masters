#include <linux/uaccess.h>
#include <linux/types.h>
#include <linux/string.h>

#include "doom_parse_cmd.h"
#include "doom_dma.h"

#define HARDDOOM_SETUP 7
#define HARDDOOM_DRAW_FUZZ 5
#define HARDDOOM_DRAW_SPAN 6


static int inline __attribute__((always_inline))
setup_base_format_type(struct cmd_format * cmd, uint8_t type)
{
        if (type == DOOMDEV2_CMD_TYPE_DRAW_SPAN) {
                cmd->type = HARDDOOM_DRAW_SPAN;
                goto ret;
        }
        if (type == DOOMDEV2_CMD_TYPE_DRAW_FUZZ) {
                cmd->type = HARDDOOM_DRAW_FUZZ;
                goto ret;
        }
        cmd->type = type;
        ret:
        return 0;
}

void parse_command_setup(
        struct cmd_setup_format *cmd_f,
        struct doom_setup *cur_setup,
        struct doom_setup *target_setup)
{
        memset(cmd_f, 0, sizeof(struct cmd_setup_format));
        cmd_f->type = HARDDOOM_SETUP;

        BUG_ON(target_setup == NULL);

        if ((cur_setup == NULL || target_setup->surf_dst != cur_setup->surf_dst) &&
            target_setup->surf_dst != NULL) {
                cmd_f->surf_dst = true;
                cmd_f->surf_dst_width = target_setup->surf_dst->width >> 6;
                cmd_f->surf_dst_pt = get_trans_table_dma_addr(
                        target_setup->surf_dst->buf);
        }

        if ((cur_setup == NULL || target_setup->surf_src != cur_setup->surf_src) &&
            target_setup->surf_src != NULL) {
                cmd_f->surf_src = true;
                cmd_f->surf_src_width = target_setup->surf_src->width >> 6;
                cmd_f->surf_src_pt = get_trans_table_dma_addr(
                        target_setup->surf_src->buf);
        }

        if ((cur_setup == NULL ||target_setup->texture != cur_setup->texture) &&
            target_setup->texture != NULL) {
                cmd_f->texture = true;
                cmd_f->texture_pt = get_trans_table_dma_addr(
                        target_setup->texture->buf);
        }

        if ((cur_setup == NULL || target_setup->flat != cur_setup->flat) &&
            target_setup->flat != NULL) {
                cmd_f->flat = true;
                cmd_f->flat_pt = get_trans_table_dma_addr(
                        target_setup->flat->buf);
        }

        if ((cur_setup == NULL || target_setup->translation != cur_setup->translation) &&
            target_setup->translation != NULL) {
                cmd_f->translation = true;
                cmd_f->translation_pt = get_trans_table_dma_addr(
                        target_setup->translation->buf);
        }

        if ((cur_setup == NULL || target_setup->colormap != cur_setup->colormap) &&
            target_setup->colormap != NULL) {
                cmd_f->colormap = true;
                cmd_f->colormap_pt = get_trans_table_dma_addr(
                        target_setup->colormap->buf);
        }

        if ((cur_setup == NULL || target_setup->tranmap != cur_setup->tranmap) &&
            target_setup->tranmap != NULL) {
                cmd_f->tranmap = true;
                cmd_f->tranmap_pt = get_trans_table_dma_addr(
                        target_setup->tranmap->buf);
        }
}

static void parse_command_copy_rect(
        struct doom_surface *src_srf,
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_copy_rect * cmd)
{
        cmd_f->x_a = cmd->pos_dst_x;
        cmd_f->y_a = cmd->pos_dst_y;
        cmd_f->x_b = cmd->pos_src_x;
        cmd_f->y_b = cmd->pos_src_y;
        cmd_f->width = cmd->width;
        cmd_f->height = cmd->height;
}

static void parse_command_fill_rect(
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_fill_rect * cmd)
{
        cmd_f->x_a = cmd->pos_x;
        cmd_f->y_a = cmd->pos_y;
        cmd_f->width = cmd->width;
        cmd_f->height = cmd->height;
        cmd_f->fill_color = cmd->fill_color;
}

static void parse_command_draw_line(
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_draw_line * cmd)
{
        cmd_f->x_a = cmd->pos_a_x;
        cmd_f->y_a = cmd->pos_a_y;
        cmd_f->x_b = cmd->pos_b_x;
        cmd_f->y_b = cmd->pos_b_y;

        cmd_f->fill_color = cmd->fill_color;
}

static void parse_command_draw_background(
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_draw_background * cmd)
{
        cmd_f->x_a = cmd->pos_x;
        cmd_f->y_a = cmd->pos_y;
        cmd_f->width = cmd->width;
        cmd_f->height = cmd->height;
        cmd_f->flat_idx = cmd->flat_idx;
}

static void parse_command_draw_column(
        struct doom_buffer *texture_buf,
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_draw_column * cmd)
{
        cmd_f->x_a = cmd->pos_x;
        cmd_f->y_a = cmd->pos_a_y;
        cmd_f->x_b = cmd->pos_x;
        cmd_f->y_b = cmd->pos_b_y;

        cmd_f->ustart = cmd->ustart;
        cmd_f->ustep = cmd->ustep;
        cmd_f->texture_offset = cmd->texture_offset;
        cmd_f->texture_height = cmd->texture_height;
        cmd_f->texture_limit = get_buffer_size(texture_buf->buf) >> 6;

        if (cmd->flags & DOOMDEV2_CMD_FLAGS_COLORMAP) {
                cmd_f->colormap = true;
                cmd_f->colormap_idx = cmd->colormap_idx;
        }

        if (cmd->flags & DOOMDEV2_CMD_FLAGS_TRANSLATE) {
                cmd_f->translation = true;
                cmd_f->translation_idx = cmd->translation_idx;
        }
}

static void parse_command_draw_fuzz(
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_draw_fuzz * cmd)
{
        cmd_f->x_a = cmd->pos_x;
        cmd_f->y_a = cmd->pos_a_y;
        cmd_f->x_b = cmd->pos_x;
        cmd_f->y_b = cmd->pos_b_y;

        cmd_f->colormap_idx = cmd->colormap_idx;
        cmd_f->fuzz_start = cmd->fuzz_start;
        cmd_f->fuzz_end = cmd->fuzz_end;
        cmd_f->fuzz_pos = cmd->fuzz_pos;
}

static void parse_command_draw_span(
        struct cmd_format *cmd_f,
        struct doomdev2_cmd_draw_span * cmd)
{
        cmd_f->x_a = cmd->pos_a_x;
        cmd_f->y_a = cmd->pos_y;
        cmd_f->x_b = cmd->pos_b_x;
        cmd_f->y_b = cmd->pos_y;

        cmd_f->ustart = cmd->ustart;
        cmd_f->vstart = cmd->vstart;
        cmd_f->ustep = cmd->ustep;
        cmd_f->vstep = cmd->vstep;

        cmd_f->flat_idx = cmd->flat_idx;

        if (cmd->flags & DOOMDEV2_CMD_FLAGS_COLORMAP) {
                cmd_f->colormap = true;
                cmd_f->colormap_idx = cmd->colormap_idx;
        }

        if (cmd->flags & DOOMDEV2_CMD_FLAGS_TRANSLATE) {
                cmd_f->translation = true;
                cmd_f->translation_idx = cmd->translation_idx;
        }

        if (cmd->flags & DOOMDEV2_CMD_FLAGS_TRANMAP)
                cmd_f->transmap = true;
}

int parse_command(struct doom_setup *setup, struct cmd_format *cmd_f,
                  struct doomdev2_cmd *cmd)
{
        memset(cmd_f, '\0', sizeof(struct cmd_format));
        setup_base_format_type(cmd_f, cmd->type);

        switch (cmd->type) {
        case DOOMDEV2_CMD_TYPE_COPY_RECT:
                parse_command_copy_rect(setup->surf_dst, cmd_f,
                (struct doomdev2_cmd_copy_rect *) cmd);
                break;
        case DOOMDEV2_CMD_TYPE_FILL_RECT:
                parse_command_fill_rect(cmd_f,
                (struct doomdev2_cmd_fill_rect *) cmd);
                break;
        case DOOMDEV2_CMD_TYPE_DRAW_LINE:
                parse_command_draw_line(cmd_f,
                (struct doomdev2_cmd_draw_line *) cmd);
                break;
        case DOOMDEV2_CMD_TYPE_DRAW_BACKGROUND:
                parse_command_draw_background(cmd_f,
                (struct doomdev2_cmd_draw_background *) cmd);
                break;
        case DOOMDEV2_CMD_TYPE_DRAW_COLUMN:
                parse_command_draw_column(setup->texture, cmd_f,
                (struct doomdev2_cmd_draw_column *) cmd);
                break;
        case DOOMDEV2_CMD_TYPE_DRAW_SPAN:
                parse_command_draw_span(cmd_f,
                (struct doomdev2_cmd_draw_span *) cmd);
                break;
        case DOOMDEV2_CMD_TYPE_DRAW_FUZZ:
                parse_command_draw_fuzz(cmd_f,
                (struct doomdev2_cmd_draw_fuzz *) cmd);
                break;
        default:
                return -EINVAL;
        }

        return 0;
}
