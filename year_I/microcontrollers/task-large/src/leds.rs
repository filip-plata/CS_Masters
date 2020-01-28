use crate::command_parser::{CommandLED, CommandLEDType};
use stm32f4::stm32f411 as stm32;
use stm32::{GPIOA, GPIOB};
use cortex_m::interrupt::{CriticalSection, Mutex, free};
use core::cell::RefCell;
use core::ops::{Deref};
use crate::config::configure_leds;

static LED_OVERSEER: Mutex<RefCell<Option<OverseerLED> >> =
    Mutex::new(RefCell::new(None));


pub(crate) struct OverseerLED {
    gpioa: GPIOA,
    gpiob: GPIOB,
}

impl OverseerLED {
    pub(crate) fn initialize(gpioa: GPIOA, gpiob: GPIOB) {
        free(|cs| {
            LED_OVERSEER.borrow(cs).replace(Some (OverseerLED {
                gpioa,
                gpiob,
            }) );
        });

        unsafe { configure_leds() };
    }
}


pub(crate) fn do_led_command(cs: &CriticalSection, cmd: CommandLED) {

    if let Some(ref led_overseer) =  LED_OVERSEER.borrow(cs).borrow().deref() {
        let gpioa = &led_overseer.gpioa;
        let gpiob = &led_overseer.gpiob;

        match (cmd.led_number, cmd.command_type) {
            (0, CommandLEDType::On) => gpioa.bsrr.write(|w| w.bs5().set_bit()),
            (0, CommandLEDType::Off) => gpioa.bsrr.write(|w| w.br5().set_bit()),
            (0, CommandLEDType::Toggle) => gpioa.odr.modify(|r, w|
                w.odr5().bit(!r.odr5().bit())
            ),

            (1, CommandLEDType::On) => gpioa.bsrr.write(|w| w.br6().set_bit()),
            (1, CommandLEDType::Off) => gpioa.bsrr.write(|w| w.bs6().set_bit()),
            (1, CommandLEDType::Toggle) => gpioa.odr.modify(|r, w|
                w.odr6().bit(!r.odr6().bit())
            ),

            (2, CommandLEDType::On) => gpioa.bsrr.write(|w| w.br7().set_bit()),
            (2, CommandLEDType::Off) => gpioa.bsrr.write(|w| w.bs7().set_bit()),
            (2, CommandLEDType::Toggle) => gpioa.odr.modify(|r, w|
                w.odr7().bit(!r.odr7().bit())
            ),

            (3, CommandLEDType::On) => gpiob.bsrr.write(|w| w.br0().set_bit()),
            (3, CommandLEDType::Off) => gpiob.bsrr.write(|w| w.bs0().set_bit()),
            (3, CommandLEDType::Toggle) => gpiob.odr.modify(|r, w|
                w.odr0().bit(!r.odr0().bit())
            ),

            _ => panic!("Software bug")
        }
    }
}