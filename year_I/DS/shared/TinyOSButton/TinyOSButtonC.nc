#include <Timer.h>
#include <UserButton.h>

/**
 * Copyright (c) 2007 Arch Rock Corporation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the Arch Rock Corporation nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * ARCHED ROCK OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE
 */

/**
 * Test for the user button on the telosb platform. Turns blue when
 * the button is pressed. Samples the button state every 4 seconds,
 * and turns green when the button is pressed.
 *
 * @author Gilman Tolle <gtolle@archrock.com>
 * @version $Revision: 1.1 $
 */

#include "printf.h"
module TinyOSButtonC @safe() {
  uses {
    interface Boot;
    interface Get<button_state_t>;
    interface Notify<button_state_t>;
    interface Leds;
    interface Timer<TMilli>;
  }
}
implementation {
  uint32_t timestamp;
  uint32_t counter = 0;

  event void Boot.booted() {
    call Notify.enable();
  }

  event void Notify.notify( button_state_t state ) {
    if ( state == BUTTON_PRESSED ) {
      printf("Button pressed.\n\r");
      timestamp = call Timer.getNow();
      call Leds.led0On();
    } else if ( state == BUTTON_RELEASED ) {
      printf("Button released after %d seconds.\n\r", (int) (call Timer.getNow() - timestamp) / 1000 );
      call Timer.stop();
      call Leds.led0Off();
    }
    printfflush();
  }

  event void Timer.fired() {
  }
}
