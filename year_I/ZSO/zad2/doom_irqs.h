#ifndef DOOM_IRQS_H
#define DOOM_IRQS_H

#include <linux/irqreturn.h>
#include "doom_device.h"

struct harddoom2_irq_callback {
        void (*fence)(struct doom_device *);
        void (*pong_sync)(struct doom_device *);
        void (*pong_async)(struct doom_device *);
        void (*fe_error)(struct doom_device *);
        void (*cmd_overflow)(struct doom_device *);
        void (*surface_overflow)(struct doom_device *);
        void (*page_fault)(struct doom_device *);
        void (*error)(struct doom_device *);
};

irqreturn_t doom_irq_handler(int irq, void *harddoom2_irq_callback);
uint32_t active_irqs(struct doom_device *);
void disable_irqs(struct doom_device *);
void enable_pong_async_irq(struct doom_device *);
void disable_pong_async_irq(struct doom_device *);
void reset_irqs(struct doom_device *);
void reset_irq(struct doom_device *, uint32_t);
void enable_irqs(struct doom_device *);

#endif /* DOOM_IRQS_H */
