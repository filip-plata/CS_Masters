#include "doom_irqs.h"
#include "debug.h"

void doom_error(struct doom_device *dev)
{
        PDEBUG("Fatal error");
        disable_irqs(dev);
}

void doom_page_fault(struct doom_device *dev)
{
        PDEBUG("Page fault");
        disable_irqs(dev);
}

void doom_fe_error(struct doom_device *dev)
{
        PDEBUG("Invalid command");
        disable_irqs(dev);
}

void doom_cmd_overflow(struct doom_device *dev)
{
        PDEBUG("Command overflow");
        disable_irqs(dev);
}

void doom_surface_overflow(struct doom_device *dev)
{
        PDEBUG("Surface overflow");
        disable_irqs(dev);
}
