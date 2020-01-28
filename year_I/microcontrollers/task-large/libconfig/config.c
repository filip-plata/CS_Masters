#include <stm32.h>
#include <gpio.h>

/* Clock start */

#define HSI_HZ 16000000U
#define PCLK2_HZ HSI_HZ

/* Clock end */

#define RED_LED_GPIO    GPIOA
#define GREEN_LED_GPIO  GPIOA
#define BLUE_LED_GPIO   GPIOB
#define GREEN2_LED_GPIO GPIOA
#define RED_LED_PIN    6
#define GREEN_LED_PIN  7
#define BLUE_LED_PIN   0
#define GREEN2_LED_PIN 5

#define RedLEDoff()    \
  RED_LED_GPIO->BSRR = 1 << RED_LED_PIN

#define GreenLEDoff()    \
  GREEN_LED_GPIO->BSRR = 1 << GREEN_LED_PIN

#define BlueLEDoff()    \
  BLUE_LED_GPIO->BSRR = 1 << BLUE_LED_PIN

#define Green2LEDoff() \
  GREEN2_LED_GPIO->BSRR = 1 << (GREEN2_LED_PIN + 16)


void configure_bluetooth() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;
    RCC->APB2ENR |= RCC_APB2ENR_USART1EN;

    __NOP();

    GPIOafConfigure(
            GPIOA,
            9,
            GPIO_OType_PP,
            GPIO_Fast_Speed,
            GPIO_PuPd_NOPULL,
            GPIO_AF_USART1);

    GPIOafConfigure(
            GPIOA,
            10,
            GPIO_OType_PP,
            GPIO_Fast_Speed,
            GPIO_PuPd_UP,
            GPIO_AF_USART1);
    USART1->CR1 = USART_CR1_RE | USART_CR1_TE;
    USART1->CR2 = 0;
    USART1->CR3 = USART_CR3_DMAT | USART_CR3_DMAR;

    uint32_t const baudrate = 9600U;
    USART1->BRR = (PCLK2_HZ + (baudrate / 2U)) / baudrate;

    USART1->CR1 |= USART_CR1_UE;
}

void configure_dma() {
    RCC->AHB1ENR |= RCC_AHB1ENR_DMA2EN;
    DMA2_Stream7->CR = 4U << 25       |
                       DMA_SxCR_PL_1  |
                       DMA_SxCR_MINC  |
                       DMA_SxCR_DIR_0 |
                       DMA_SxCR_TCIE;
    DMA2_Stream7->PAR = (uint32_t) &USART1->DR;

    DMA2_Stream5->CR = 4U << 25       |
                       DMA_SxCR_PL_1  |
                       DMA_SxCR_MINC  |
                       DMA_SxCR_TCIE;
    DMA2_Stream5->PAR = (uint32_t) &USART1->DR;

    DMA2->HIFCR = DMA_HIFCR_CTCIF7 | DMA_HIFCR_CTCIF5;
    NVIC_EnableIRQ(DMA2_Stream5_IRQn);
    NVIC_EnableIRQ(DMA2_Stream7_IRQn);
}

void configure_leds() {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_GPIOBEN | RCC_AHB1ENR_GPIOCEN;
    __NOP();

    RedLEDoff();
    BlueLEDoff();
    GreenLEDoff();
    Green2LEDoff();

    GPIOoutConfigure(
            RED_LED_GPIO,
            RED_LED_PIN,
            GPIO_OType_PP,
            GPIO_Low_Speed,
            GPIO_PuPd_NOPULL);

    GPIOoutConfigure(
            BLUE_LED_GPIO,
            BLUE_LED_PIN,
            GPIO_OType_PP,
            GPIO_Low_Speed,
            GPIO_PuPd_NOPULL);

    GPIOoutConfigure(
            GREEN_LED_GPIO,
            GREEN_LED_PIN,
            GPIO_OType_PP,
            GPIO_Low_Speed,
            GPIO_PuPd_NOPULL);

    GPIOoutConfigure(
            GREEN2_LED_GPIO,
            GREEN2_LED_PIN,
            GPIO_OType_PP,
            GPIO_Low_Speed,
            GPIO_PuPd_NOPULL);
}

void clear_exti() {
    EXTI->PR = EXTI_PR_PR5 | EXTI_PR_PR4;
}

void awaiting_stable_encoder() {
    NVIC_DisableIRQ(EXTI4_IRQn);
    NVIC_DisableIRQ(EXTI9_5_IRQn);
}

void encoder_stable() {
    clear_exti();

    NVIC_EnableIRQ(EXTI4_IRQn);
    NVIC_EnableIRQ(EXTI9_5_IRQn);
}

void configure_rotational_encoder() {
    RCC->APB1ENR |= RCC_APB1ENR_TIM3EN | RCC_APB1ENR_TIM2EN;
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOBEN;
    RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN;

    TIM3->CR1 = TIM_CR1_URS;
    TIM3->SMCR = TIM_SMCR_SMS_1 | TIM_SMCR_SMS_0;

    TIM3->CCMR1 = TIM_CCMR1_CC1S_0 |
                  TIM_CCMR1_CC2S_0 |
                  TIM_CCMR1_IC1F_0 |
                  TIM_CCMR1_IC1F_1 |
                  TIM_CCMR1_IC2F_0 |
                  TIM_CCMR1_IC2F_1;

    TIM3->CNT = 0;
    TIM3->CCER = 0;
    TIM3->PSC = 0;
    TIM3->ARR = 191;
    TIM3->EGR = TIM_EGR_UG;

    TIM2->PSC = 15999;
    TIM2->CR1 = 0;
    TIM2->CNT = 0;
    TIM2->ARR = 5;
    TIM2->EGR = TIM_EGR_UG;

    GPIOinConfigure(
            GPIOB,
            4,
            GPIO_PuPd_NOPULL,
            EXTI_Mode_Interrupt,
            EXTI_Trigger_Rising_Falling);

    GPIOinConfigure(
            GPIOB,
            5,
            GPIO_PuPd_NOPULL,
            EXTI_Mode_Interrupt,
            EXTI_Trigger_Rising_Falling);

    GPIOafConfigure(
            GPIOB,
            4,
            GPIO_OType_PP,
            GPIO_Low_Speed,
            GPIO_PuPd_UP,
            GPIO_AF_TIM3);

    GPIOafConfigure(
            GPIOB,
            5,
            GPIO_OType_PP,
            GPIO_Low_Speed,
            GPIO_PuPd_UP,
            GPIO_AF_TIM3);

    TIM3->CR1 |= TIM_CR1_CEN;

    encoder_stable();

    TIM2->SR = ~TIM_SR_UIF;
    TIM2->DIER = TIM_DIER_UIE;
    NVIC_EnableIRQ(TIM2_IRQn);
}
