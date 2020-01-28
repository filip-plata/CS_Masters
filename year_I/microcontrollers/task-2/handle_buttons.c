#include <stm32.h>
#include <gpio.h>
#include <stddef.h>
#include <stdbool.h>

#include "handle_buttons.h"
#include "uart.h"


#define X_pressed(BUTTON_NAME, ADDR_SPACE, N, TARGET) \
  bool button_##BUTTON_NAME##_pressed() {\
    return (ADDR_SPACE->IDR & (1 << N)) == TARGET;\
  }

X_pressed(user, GPIOC, 13, 0);

#ifdef EXPANDER
X_pressed(left, GPIOB, 3, 0);
X_pressed(right, GPIOB, 4, 0);
X_pressed(up, GPIOB, 5, 0);
X_pressed(down, GPIOB, 6, 0);
X_pressed(fire, GPIOB, 10, 0);
X_pressed(mode, GPIOA, 0, 1);
#endif


static void configureButton_PC13();
#ifdef EXPANDER
static void configureButton_left();
static void configureButton_right();
static void configureButton_up();
static void configureButton_down();
static void configureButton_fire();
static void configureButton_mode();
#endif


void configureButtons(void) {
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN | RCC_AHB1ENR_GPIOBEN | RCC_AHB1ENR_GPIOCEN;
    __NOP();
    RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN;
    configureButton_PC13();
#ifdef EXPANDER
    configureButton_left();
    configureButton_right();
    configureButton_up();
    configureButton_down();
    configureButton_fire();
    configureButton_mode();

    NVIC_EnableIRQ(EXTI0_IRQn);
    NVIC_EnableIRQ(EXTI9_5_IRQn);
    NVIC_EnableIRQ(EXTI4_IRQn);
    NVIC_EnableIRQ(EXTI3_IRQn);
#endif
    NVIC_EnableIRQ(EXTI15_10_IRQn);
}

#ifdef EXPANDER

void EXTI0_IRQHandler() {
    EXTI->PR = EXTI_PR_PR0;
    print_str(button_mode_pressed() ? "MODE_PRESSED\r\n" : "MODE_RELEASED\r\n");
}

void EXTI3_IRQHandler() {
    EXTI->PR = EXTI_PR_PR3;
    print_str(button_left_pressed() ? "LEFT_PRESSED\r\n" : "LEFT_RELEASED\r\n");
}

void EXTI4_IRQHandler() {
    EXTI->PR = EXTI_PR_PR4;
    print_str(button_right_pressed() ? "RIGHT_PRESSED\r\n" : "RIGHT_RELEASED\r\n");
}

void EXTI9_5_IRQHandler() {
    if (EXTI->PR & EXTI_PR_PR5) {
        EXTI->PR = EXTI_PR_PR5;
        print_str(button_up_pressed() ? "UP_PRESSED\r\n" : "UP_RELEASED\r\n");
    }

    if (EXTI->PR & EXTI_PR_PR6) {
        EXTI->PR = EXTI_PR_PR6;
        print_str(button_down_pressed() ? "DOWN_PRESSED\r\n" : "DOWN_RELEASED\r\n");
    }
}

#endif

void EXTI15_10_IRQHandler() {
    if (EXTI->PR & EXTI_PR_PR13) {
        EXTI->PR = EXTI_PR_PR13;
        print_str(button_user_pressed() ? "USER_PRESSED\r\n" : "USER_RELEASED\r\n");
    }

    #ifdef EXPANDER
    if (EXTI->PR & EXTI_PR_PR10) {
        EXTI->PR = EXTI_PR_PR10;
        print_str(button_fire_pressed() ? "FIRE_PRESSED\r\n" : "FIRE_RELEASED\r\n");
    }
    #endif
}


static void configureButton_PC13() {
    GPIOinConfigure(
        GPIOC,
        13,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );
    EXTI->PR = EXTI_PR_PR13;
}

#ifdef EXPANDER

static void configureButton_left() {
    GPIOinConfigure(
        GPIOB,
        3,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );

    EXTI->PR = EXTI_PR_PR3;
}

static void configureButton_right() {
    GPIOinConfigure(
        GPIOB,
        4,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );

    EXTI->PR = EXTI_PR_PR4;
}

static void configureButton_up() {
    GPIOinConfigure(
        GPIOB,
        5,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );

    EXTI->PR = EXTI_PR_PR5;
}

static void configureButton_down() {
    GPIOinConfigure(
        GPIOB,
        6,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );

    EXTI->PR = EXTI_PR_PR6;
}

static void configureButton_fire() {
    GPIOinConfigure(
        GPIOB,
        10,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );

    EXTI->PR = EXTI_PR_PR10;
}

static void configureButton_mode() {
    GPIOinConfigure(
        GPIOA,
        0,
        GPIO_PuPd_UP,
        EXTI_Mode_Interrupt,
        EXTI_Trigger_Rising_Falling
    );
    EXTI->PR = EXTI_PR_PR0;
}

#endif /* EXPANDER */
