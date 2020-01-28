#ifndef HANDLE_COMMAND_H
#define HANDLE_COMAND_H

#include <stdbool.h>

#define RED_LED_GPIO    GPIOA
#define GREEN_LED_GPIO  GPIOA
#define BLUE_LED_GPIO   GPIOB
#define GREEN2_LED_GPIO GPIOA
#define RED_LED_PIN    6
#define GREEN_LED_PIN  7
#define BLUE_LED_PIN   0
#define GREEN2_LED_PIN 5

#define RedLEDon()     \
  RED_LED_GPIO->BSRR = 1 << (RED_LED_PIN + 16)
#define RedLEDoff()    \
  RED_LED_GPIO->BSRR = 1 << RED_LED_PIN
#define RedLEDtoggle() \
  RED_LED_GPIO->ODR ^= (1 << RED_LED_PIN)

#define GreenLEDon()     \
  GREEN_LED_GPIO->BSRR = 1 << (GREEN_LED_PIN + 16)
#define GreenLEDoff()    \
  GREEN_LED_GPIO->BSRR = 1 << GREEN_LED_PIN
#define GreenLEDtoggle() \
  GREEN_LED_GPIO->ODR ^= (1 << GREEN_LED_PIN)

#define BlueLEDon()     \
  BLUE_LED_GPIO->BSRR = 1 << (BLUE_LED_PIN + 16)
#define BlueLEDoff()    \
  BLUE_LED_GPIO->BSRR = 1 << BLUE_LED_PIN
#define BlueLEDtoggle() \
  BLUE_LED_GPIO->ODR ^= (1 << BLUE_LED_PIN)

#define Green2LEDon()  \
  GREEN2_LED_GPIO->BSRR = 1 << GREEN2_LED_PIN
#define Green2LEDoff() \
  GREEN2_LED_GPIO->BSRR = 1 << (GREEN2_LED_PIN + 16)
#define Green2LEDtoggle() \
  GREEN2_LED_GPIO->ODR ^= (1 << GREEN2_LED_PIN)

enum command_type {
  LED_OFF,
  LED_ON,
  LED_TOGGLE
};


struct command {
  enum command_type type;
  short int number;
};

void configure_leds();
bool parse_command(struct command*, char);
void do_command(struct command *);

#endif /* HANDLE_BUTTONS_H */
