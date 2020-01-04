#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include "doomdev2.h"


int main() {
        char buf[64];
        struct doomdev2_ioctl_create_surface param1 = {.width = 64, .height = 10};
        struct doomdev2_ioctl_create_buffer param2 = {.size = 8192};
        struct doomdev2_ioctl_create_buffer param3 = {.size = 4096};
        struct doomdev2_ioctl_create_buffer param_tranmap = {.size = 65536};
        struct doomdev2_ioctl_setup setup = {
                .surf_dst_fd = -1,
                .surf_src_fd = -1,
                .texture_fd = -1,
                .flat_fd = -1,
                .colormap_fd = -1,
                .translation_fd = -1,
                .tranmap_fd = -1,
        };
        struct doomdev2_cmd_fill_rect cmds[2];
        struct doomdev2_cmd_fill_rect cmd = {
        	.type = DOOMDEV2_CMD_TYPE_FILL_RECT,
        	.fill_color = 42,
        	.width = 10,
        	.height = 10,
        	.pos_x = 0,
        	.pos_y = 0,
        };
        struct doomdev2_cmd_draw_line line = {
                .type = DOOMDEV2_CMD_TYPE_DRAW_LINE,
        	.fill_color = 17,
        	.pos_a_x = 0,
        	.pos_a_y = 0,
        	.pos_b_x = 6,
        	.pos_b_y = 0,
        };
        struct doomdev2_cmd_draw_column column = {
                .type = DOOMDEV2_CMD_TYPE_DRAW_COLUMN,
        	.flags = 0,
        	.pos_x = 0,
        	.pos_a_y = 0,
        	.pos_b_y = 1,
        	.texture_height = 2,
        	.texture_offset = 0,
                .colormap_idx = 0,
                .translation_idx = 0,
        	.ustart = 0,
        	.ustep = 0,
        };
        struct doomdev2_cmd_draw_background bg = {
        	.type = DOOMDEV2_CMD_TYPE_DRAW_BACKGROUND,
        	.flat_idx = 0,
        	.width = 64,
        	.height = 10,
        	.pos_x = 0,
        	.pos_y = 0,
        };
        struct doomdev2_cmd_copy_rect copy = {
        	.type = DOOMDEV2_CMD_TYPE_COPY_RECT,
        	.width = 4,
        	.height = 1,
        	.pos_dst_x = 4,
        	.pos_dst_y = 1,
        	.pos_src_x = 0,
        	.pos_src_y = 0,
        };
        struct doomdev2_cmd_draw_fuzz fuzz = {
        	.type = DOOMDEV2_CMD_TYPE_DRAW_FUZZ,
        	.fuzz_pos = 42,
        	.pos_x = 0,
        	.pos_a_y = 0,
        	.pos_b_y = 1,
        	.fuzz_start = 0,
        	.fuzz_end = 2,
        	.colormap_idx = 1,
        };
        int harddoom = open("/dev/doom0", O_RDWR);
        int surf_dst = ioctl(harddoom, DOOMDEV2_IOCTL_CREATE_SURFACE, &param1);
        int surf_src = ioctl(harddoom,DOOMDEV2_IOCTL_CREATE_SURFACE, &param1);
        int flat = ioctl(harddoom, DOOMDEV2_IOCTL_CREATE_TEXTURE, &param3);
        int texture = ioctl(harddoom, DOOMDEV2_IOCTL_CREATE_TEXTURE, &param2);
        int colormap = ioctl(harddoom, DOOMDEV2_IOCTL_CREATE_TEXTURE, &param2);
        int translation = ioctl(harddoom, DOOMDEV2_IOCTL_CREATE_TEXTURE, &param2);
        int tranmap = ioctl(harddoom, DOOMDEV2_IOCTL_CREATE_TEXTURE, &param_tranmap);

        cmds[0] = cmd;
        cmds[1] = cmd;
        setup.surf_dst_fd = surf_dst;
        setup.surf_src_fd = surf_dst;
        setup.flat_fd = flat;
        setup.colormap_fd = colormap;
        setup.texture_fd = texture;
        setup.translation_fd = translation;
        setup.tranmap_fd = tranmap;
        printf("Surface dst %d surface src %d flat %d\n", surf_dst, surf_src, flat);
        ioctl(harddoom, DOOMDEV2_IOCTL_SETUP, &setup);
        //for (int i = 0; i < 2; i++)
        //        printf("Write res %ld\n", write(harddoom, cmds, 2*sizeof(struct doomdev2_cmd)));
        printf("Write res %ld\n", write(harddoom, &line, sizeof(struct doomdev2_cmd)));
        printf("Write res %ld\n", write(harddoom, &copy, sizeof(struct doomdev2_cmd)));
        //printf("Write res %ld\n", write(harddoom, &bg, sizeof(struct doomdev2_cmd)));
        //printf("Write res %ld\n", write(harddoom, &fuzz, sizeof(struct doomdev2_cmd)));
        //printf("Write res %ld\n", write(harddoom, &column, sizeof(struct doomdev2_cmd)));
        lseek(surf_dst, 0, SEEK_SET);
        for (int j=0; j < 10; j++) {
                read(surf_dst, &buf, 64);
                for (int i=0; i<64; i++)
                        printf("%d", buf[i]);

                printf("\n");
        }
        return 0;
}
