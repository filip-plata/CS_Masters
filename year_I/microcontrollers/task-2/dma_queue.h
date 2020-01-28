#ifndef DMA_QUEUE_H
#define DMA_QUEUE_H

#include <stdbool.h>

struct dma_request {
    char *c;
};

void put_request(struct dma_request);

#endif
