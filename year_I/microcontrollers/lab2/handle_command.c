#include <gpio.h>
#include <stm32.h>
#include <string.h>

#include "handle_command.h"


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

enum parse_state {
    PARSE_START,
    PARSE_NUM,
    PARSE_TYPE
};


bool parse_command(struct command *cmd, char c) {
  static enum parse_state state = PARSE_START;

  switch (state) {
      case PARSE_START:
          if (c == 'l')
              state = PARSE_NUM;
          return false;
      case PARSE_NUM:
          cmd->number = c - '0';
          if (cmd->number < 0 || cmd->number > 3) {
              state = PARSE_START;
              return false;
          }
          state = PARSE_TYPE;
          return false;
      case PARSE_TYPE:
          state = PARSE_START; // state always returns to beginning

          if (c == 't') {
              cmd->type = LED_TOGGLE;
              return true;
          }

          if (c == 'f') {
              cmd->type = LED_OFF;
              return true;
          }

          if (c == 'o') {
              cmd->type = LED_ON;
              return true;
          }

          return false;
      default:
          return false;
  }
}

void do_command(struct command *cmd) {
    switch (cmd->number) {
      case 0:
          if (cmd->type == LED_ON)
              RedLEDon();
          if (cmd->type == LED_OFF)
              RedLEDoff();
          if (cmd->type == LED_TOGGLE)
              RedLEDtoggle();
          break;
      case 1:
          if (cmd->type == LED_ON)
              Green2LEDon();
          if (cmd->type == LED_OFF)
              Green2LEDoff();
          if (cmd->type == LED_TOGGLE)
              Green2LEDtoggle();
          break;
      case 2:
          if (cmd->type == LED_ON)
              GreenLEDon();
          if (cmd->type == LED_OFF)
              GreenLEDoff();
          if (cmd->type == LED_TOGGLE)
              GreenLEDtoggle();
          break;
      case 3:
          if (cmd->type == LED_ON)
              BlueLEDon();
          if (cmd->type == LED_OFF)
              BlueLEDoff();
          if (cmd->type == LED_TOGGLE)
              BlueLEDtoggle();
          break;
      default:
          break;
    }
}
