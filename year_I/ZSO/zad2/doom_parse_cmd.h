#ifndef HARDDOOM_PARSE_CMD_H
#define HARDDOOM_PARSE_CMD_H

#include "doomdev2.h"
#include "doom_common.h"

struct cmd_format {
        /* word 0 */
        uint8_t type : 4;
        bool interlock : 1;
        bool ping_async : 1;
        bool ping_sync : 1;
        bool fence : 1;
        bool translation : 1;
        bool colormap : 1;
        bool transmap : 1;
        uint32_t padding1 : 21;

        /* word 1 */
        uint16_t translation_idx : 14;
        char padding2 : 2;
        uint16_t colormap_idx : 14;
        char padding3 : 2;

        /* word 2 */
        uint16_t x_a : 11;
        uint16_t y_a : 11;
        uint16_t flat_idx : 10;

        /* word 3 */
        uint16_t x_b : 11;
        uint16_t y_b : 11;
        uint16_t padding4 : 10;

        /* word 4 */
        uint32_t ustart;

        /* word 5 */
        uint32_t ustep;

        /* word 6 */
        union {
                struct {
                        uint16_t width : 12;
                        uint16_t height : 12;
                        uint16_t fill_color : 8;
                } __attribute__((__packed__));
                struct {
                        uint32_t texture_offset : 22;
                        uint16_t padding5 : 10;
                } __attribute__((__packed__));
                uint32_t vstart;
                struct {
                        uint16_t fuzz_start : 11;
                        char padding6 : 1;
                        uint16_t fuzz_end : 11;
                        char padding7 : 1;
                        uint8_t fuzz_pos : 6;
                        char padding8 : 2;
                } __attribute__((__packed__));
        };

        /* word 7 */
        union {
                struct {
                        uint16_t texture_limit;
                        uint16_t texture_height;
                } __attribute__((__packed__));
                uint32_t vstep;
        };
} __attribute__((__packed__));

struct cmd_setup_format {
        /* word 0 */
        uint8_t type : 4;
        bool interlock : 1;
        bool ping_async : 1;
        bool ping_sync : 1;
        bool fence : 1;
        char paddington : 1;
        bool surf_dst : 1;
        bool surf_src : 1;
        bool texture : 1;
        bool flat : 1;
        bool translation : 1;
        bool colormap : 1;
        bool tranmap : 1;
        uint16_t surf_dst_width : 6;
        char padding1 : 2;
        uint16_t surf_src_width : 6;
        char padding2 : 2;

        /* word 1 */
        uint32_t surf_dst_pt;
        /* word 2 */
        uint32_t surf_src_pt;
        /* word 3 */
        uint32_t texture_pt;
        /* word 4 */
        uint32_t flat_pt;
        /* word 5 */
        uint32_t translation_pt;
        /* word 6 */
        uint32_t colormap_pt;
        /* word 7 */
        uint32_t tranmap_pt;
} __attribute__((__packed__));

struct cmd_format_all {
        union {
                struct {
                        uint8_t type : 4;
                        bool interlock : 1;
                        bool ping_async : 1;
                        bool ping_sync : 1;
                        bool fence : 1;
                } __attribute__((__packed__));
                struct cmd_format cmd_format_;
                struct cmd_setup_format cmd_format_all_;
        };
} __attribute__((__packed__));

int parse_command(struct doom_setup *, struct cmd_format *, struct doomdev2_cmd *);
void parse_command_setup(struct cmd_setup_format *, struct doom_setup *,
        struct doom_setup *);


_Static_assert (sizeof(struct cmd_format) == 32, "Comand invalid length");
_Static_assert (sizeof(struct cmd_setup_format) == 32, "Comand invalid length");
_Static_assert (sizeof(struct cmd_format_all) == 32, "Comand invalid length");

#endif /* HARDDOOM_PARSE_CMD_H */
