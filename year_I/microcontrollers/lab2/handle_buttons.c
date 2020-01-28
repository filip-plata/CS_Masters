#include <stm32.h>
#include <gpio.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "handle_buttons.h"


extern void print(char *c, int);

#define X_pressed(BUTTON_NAME, ADDR_SPACE, N, TARGET) \
  bool button_##BUTTON_NAME##_pressed() {\
    return (ADDR_SPACE->IDR & (1 << N)) == TARGET;\
  }

// może przesunąć numery w dół o jeden
X_pressed(user, GPIOC, 13, 0);
X_pressed(left, GPIOB, 3, 0);
X_pressed(right, GPIOB, 4, 0);
X_pressed(up, GPIOB, 5, 0);
X_pressed(down, GPIOB, 6, 0);
X_pressed(fire, GPIOB, 10, 0);
X_pressed(mode, GPIOA, 0, 1);

void check_buttons(struct buttons_state *buttons) {
    char *msg = NULL;

    if (buttons->user_pressed != button_user_pressed()) {
        buttons->user_pressed = button_user_pressed();
        msg = buttons->user_pressed ? "USER_PRESSED\r\n" : "USER_RELEASED\r\n";
    }

    if (buttons->left_pressed != button_left_pressed()) {
        buttons->left_pressed = button_left_pressed();
        msg = buttons->left_pressed ? "LEFT_PRESSED\r\n" : "LEFT_RELEASED\r\n";
    }

    if (buttons->right_pressed != button_right_pressed()) {
        buttons->right_pressed = button_right_pressed();
        msg = buttons->right_pressed ? "RIGHT_PRESSED\r\n" : "RIGHT_RELEASED\r\n";
    }

    if (buttons->up_pressed != button_up_pressed()) {
        buttons->up_pressed = button_up_pressed();
        msg = buttons->up_pressed ? "UP_PRESSED\r\n" : "UP_RELEASED\r\n";
    }

    if (buttons->down_pressed != button_down_pressed()) {
        buttons->down_pressed = button_down_pressed();
        msg = buttons->down_pressed ? "DOWN_PRESSED\r\n" : "DOWN_RELEASED\r\n";
    }

    if (buttons->fire_pressed != button_fire_pressed()) {
        buttons->fire_pressed = button_fire_pressed();
        msg = buttons->fire_pressed ? "FIRE_PRESSED\r\n" : "FIRE_RELEASED\r\n";
    }

    if (buttons->mode_pressed != button_mode_pressed()) {
        buttons->mode_pressed = button_mode_pressed();
        msg = buttons->mode_pressed ? "MODE_PRESSED\r\n" : "MODE_RELEASED\r\n";
    }

    if (msg != NULL)
        print(msg, strlen(msg));
}
