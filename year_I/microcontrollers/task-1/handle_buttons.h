#ifndef HANDLE_BUTTONS_H
#define HANDLE_BUTTONS_H

#include <stdbool.h>

struct buttons_state {
  bool left_pressed;
  bool right_pressed;
  bool up_pressed;
  bool down_pressed;
  bool fire_pressed;
  bool user_pressed;
  bool mode_pressed;
};

void check_buttons(struct buttons_state *);

#endif /* HANDLE_BUTTONS_H */
