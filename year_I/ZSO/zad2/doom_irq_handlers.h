#ifndef DOOM_IRQ_HANDLERS_H
#define DOOM_IRQ_HANDLERS_H

#include "doom_device.h"

void doom_error(struct doom_device *);
void doom_page_fault(struct doom_device *);
void doom_fe_error(struct doom_device *);
void doom_cmd_overflow(struct doom_device *);
void doom_surface_overflow(struct doom_device *);

#endif /* DOOM_IRQ_HANDLERS */
