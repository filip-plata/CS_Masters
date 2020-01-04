#ifndef DOOM_REG_H
#define DOOM_REG_H

#include <linux/pci.h>
#include "doom_common.h"

int doom_register_irqs(struct doom_device *);
void doom_free_irqs(struct doom_device *);

int pci_register_doom_device(struct pci_dev *, struct doom_device *);
void pci_deregister_doom_device(struct doom_device *);

int platform_register(
        struct doom_device *, dev_t, struct file_operations *, struct class *);
void platform_deregister(struct doom_device *, dev_t, struct class *);

#endif /* DOOM_REG_H */
