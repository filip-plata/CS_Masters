#include <stm32.h>
#include <gpio.h>
#include <string.h>

#include "dma_queue.h"
#include "uart.h"


/* CR1 register start */

#define USART_Mode_Rx_Tx (USART_CR1_RE | \
    USART_CR1_TE)
#define USART_Enable     USART_CR1_UE

#define USART_WordLength_8b 0x0000
#define USART_WordLength_9b USART_CR1_M

#define USART_Parity_No   0x0000
#define USART_Parity_Even USART_CR1_PCE
#define USART_Parity_Odd  (USART_CR1_PCE | \
  USART_CR1_PS)

/* CR1 register end */

/* CR2 register start */

#define USART_StopBits_1   0x0000
#define USART_StopBits_0_5 0x1000
#define USART_StopBits_2   0x2000
#define USART_StopBits_1_5 0x3000

/* CR2 register end */

/* CR3 register start */

#define USART_FlowControl_None 0x0000
#define USART_FlowControl_RTS  USART_CR3_RTSE
#define USART_FlowControl_CTS  USART_CR3_CTSE

/* CR3 register end */

/* Clock start */

#define HSI_HZ 16000000U
#define PCLK1_HZ HSI_HZ

/* Clock end */


static void configureDMA();


void print_str(char *c) {
    put_request((struct dma_request) {c});
}


void configureUART() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;
    RCC->APB1ENR |= RCC_APB1ENR_USART2EN;

    __NOP();

    GPIOafConfigure(
      GPIOA,
      2,
      GPIO_OType_PP,
      GPIO_Fast_Speed,
      GPIO_PuPd_NOPULL,
      GPIO_AF_USART2);

    GPIOafConfigure(
      GPIOA,
      3,
      GPIO_OType_PP,
      GPIO_Fast_Speed,
      GPIO_PuPd_UP,
      GPIO_AF_USART2);

    USART2->CR1 = USART_CR1_RE | USART_CR1_TE;
    USART2->CR2 = 0;
    USART2->CR3 = USART_CR3_DMAT | USART_CR3_DMAR;

    uint32_t const baudrate = 9600U;
    USART2->BRR = (PCLK1_HZ + (baudrate / 2U)) / baudrate;

    configureDMA();
    USART2->CR1 |= USART_CR1_UE;
}

static void configureDMA() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_DMA1EN;
    DMA1_Stream6->CR = 4U << 25       |
                       DMA_SxCR_PL_1  |
                       DMA_SxCR_MINC  |
                       DMA_SxCR_DIR_0 |
                       DMA_SxCR_TCIE;
    DMA1_Stream6->PAR = (uint32_t) &USART2->DR;
    DMA1->HIFCR = DMA_HIFCR_CTCIF6;
    NVIC_EnableIRQ(DMA1_Stream6_IRQn);
}
