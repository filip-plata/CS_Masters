#include "handle_buttons.h"
#include "uart.h"


int main() {
    configureUART();
    configureButtons();

    while (1) ;
}
