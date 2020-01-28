#include <stm32.h>
#include <irq.h>
#include <string.h>

#include "dma_queue.h"

#define QUEUE_SIZE 256

static volatile unsigned char queue_write = 0;
static volatile unsigned char queue_read = 0;

static struct dma_request queue[QUEUE_SIZE];

static void handle_uart_dma(struct dma_request);


void put_request(struct dma_request r) {
      irq_level_t level = IRQprotectAll();

      if (queue_write + 1 == queue_read) {
          IRQunprotectAll(level);
          return;
      }
      queue[queue_write++] = r;

      if ((DMA1_Stream6->CR & DMA_SxCR_EN) == 0 &&
          (DMA1->HISR & DMA_HISR_TCIF6) == 0) {
          handle_uart_dma(r);
      }

      IRQunprotectAll(level);
}

void DMA1_Stream6_IRQHandler() {
      irq_level_t level = IRQprotectAll();

      uint32_t isr = DMA1->HISR;
      if (isr & DMA_HISR_TCIF6) {
            queue_read++;

            DMA1->HIFCR = DMA_HIFCR_CTCIF6;

            if (queue_read != queue_write)
                handle_uart_dma(queue[queue_read]);
      }

      IRQunprotectAll(level);
}

static void handle_uart_dma(struct dma_request r) {
    DMA1_Stream6->M0AR = (uint32_t) r.c;
    DMA1_Stream6->NDTR = strlen(r.c);
    DMA1_Stream6->CR |= DMA_SxCR_EN;
}
