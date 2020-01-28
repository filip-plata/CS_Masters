use crate::config::{configure_rotational_encoder, awaiting_stable_encoder, encoder_stable};

use core::cell::{RefCell, Cell};
use core::ops::DerefMut;

use stm32f4::stm32f411 as stm32;
use stm32::{TIM2, TIM3, EXTI, interrupt};

use cortex_m::interrupt::{free, Mutex};

use heapless::{String, Vec};
use heapless::consts::{U16, U3};

use crate::dma::{DmaSubsystem, DmaRequest};

const ENCODER_START_VALUE: u16 = 0;
static ENCODER_SUBSYSTEM: Mutex<RefCell<Option< RotationalEncoderSubsystem >>> =
    Mutex::new(RefCell::new(None));

static CTR: Mutex<Cell<u16>> = Mutex::new(Cell::new(ENCODER_START_VALUE));


pub(crate) struct RotationalEncoderSubsystem {
    tim3: TIM3,
    tim2: TIM2,
    exti: EXTI,
}


pub(crate) fn initialize(tim2: TIM2, tim3: TIM3, exti: EXTI) {
    let encoder = RotationalEncoderSubsystem {
        tim2,
        tim3,
        exti,
    };

    free(|cs| {
        ENCODER_SUBSYSTEM.borrow(cs).replace(Some (encoder) );
    });

    unsafe { configure_rotational_encoder() };
}


#[interrupt]
fn EXTI4() {
    rotation_started_interrupt();
}

#[interrupt]
fn EXTI9_5() {
    rotation_started_interrupt();
}

#[interrupt]
fn TIM2() {
    static OLD_CTR: Mutex<Cell<u16>> = Mutex::new(Cell::new(ENCODER_START_VALUE));
    static LAST_SENT: Mutex<Cell<u16>> = Mutex::new(Cell::new(ENCODER_START_VALUE));

    free(|cs| {
        let (new_ctr, old_ctr, last) =
            (CTR.borrow(cs).get(), OLD_CTR.borrow(cs), LAST_SENT.borrow(cs));

        if let Some(ref mut encoder) = ENCODER_SUBSYSTEM.borrow(cs).borrow_mut().deref_mut() {
            encoder.tim2.sr.modify(|_, w| w.uif().clear());

            if new_ctr != old_ctr.get() {
                old_ctr.replace(new_ctr);
                return
            }

            if new_ctr != last.get() {
                let msg = encoder_message(new_ctr);
                DmaSubsystem::put_dma_request(DmaRequest::build(msg));
                last.set(new_ctr);
            }

            encoder.tim2.cr1.modify(|_, w| w.cen().disabled());
            unsafe { encoder_stable() };
        }
    });
}

fn rotation_started_interrupt() {
    free(|cs| {
        if let Some(ref mut encoder) = ENCODER_SUBSYSTEM.borrow(cs).borrow_mut().deref_mut() {
            encoder.exti.pr.modify(|_, w| w.pr4().set_bit().pr5().set_bit());

            CTR.borrow(cs).replace(encoder.tim3.cnt.read().cnt().bits());

            encoder.tim2.cnt.write(|w| w.cnt().bits(0));
            encoder.tim2.sr.modify(|_, w| w.uif().clear());

            encoder.tim2.cr1.modify(|_, w| w.cen().enabled());
            unsafe { awaiting_stable_encoder() };
        }
    });
}

fn encoder_message(mut ctr: u16) -> String<U16> {
    let mut s: String<U16> = String::from("P");
    let mut digits: Vec<u8, U3> = Vec::new();

    while {
        digits.push((ctr % 10) as u8).unwrap();
        ctr /= 10;

        ctr != 0
    } {}

    for digit in digits.iter().rev() {
        s.push(char::from(digit + 48)).unwrap();
    }

    s.push('\r').unwrap();
    s.push('\n').unwrap();

    s
}
