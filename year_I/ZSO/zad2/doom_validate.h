#ifndef HARDDOOM_VALIDATE_H
#define HARDDOOM_VALIDATE_H

#include "doom_common.h"
#include "doomdev2.h"

int validate_cmds_batch(struct doom_setup *, struct doomdev2_cmd *, size_t);
int validate_setup_cmd(struct doom_setup *);
int validate_create_surface(struct doomdev2_ioctl_create_surface *);
int validate_create_buffer(struct doomdev2_ioctl_create_buffer *);

#endif /* HARDDOOM_VALIDATE_H */
