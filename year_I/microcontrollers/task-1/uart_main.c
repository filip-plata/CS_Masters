#include <delay.h>
#include <stm32.h>
#include <gpio.h>
#include <stdbool.h>
#include <string.h>

#include "handle_buttons.h"
#include "handle_command.h"

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


#define BUF_SEND_SIZE 1024
#define BUF_REC_SIZE 1024

static char send_buffer[BUF_SEND_SIZE];
static int send_buffer_pos_write = 0;
static int send_buffer_pos_read = 0;


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

    USART2->CR1 = USART_Mode_Rx_Tx | USART_WordLength_8b | USART_Parity_No;
    USART2->CR2 = USART_StopBits_1;
    USART2->CR3 = USART_FlowControl_None;

    uint32_t const baudrate = 9600U;
    USART2->BRR = (PCLK1_HZ + (baudrate / 2U)) / baudrate;
    USART2->CR1 |= USART_Enable;
}


void print(char *c, int len) {
    while (len--) {
        send_buffer[send_buffer_pos_write++] = *c++;
        send_buffer_pos_write %= BUF_SEND_SIZE;
    }
}


bool read(struct command *cmd) {
    if (!(USART2->SR & USART_SR_RXNE))
        return false;

    return parse_command(cmd, USART2->DR);
}


void try_put() {
    if (!(USART2->SR & USART_SR_TXE) || send_buffer_pos_read == send_buffer_pos_write)
        return;

    USART2->DR = send_buffer[send_buffer_pos_read++];
    send_buffer_pos_read %= BUF_SEND_SIZE;
}


int main() {
    struct command cmd;
    struct buttons_state buttons;

    memset(&buttons, 0, sizeof(struct buttons_state));
    configure_leds();
    configureUART();

    while (true) {
        if (read(&cmd))
            do_command(&cmd);

        check_buttons(&buttons);

        try_put();
    }
}
