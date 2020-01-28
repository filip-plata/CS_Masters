#![no_main]
#![no_std]

extern crate panic_halt;


use cortex_m_rt::entry;

use stm32f4::stm32f411 as stm32;
use crate::leds::OverseerLED;
use crate::command_parser::CommandParser;
use crate::config::configure_bluetooth;

mod dma;
mod config;
mod command_parser;
mod leds;
mod rotational_encoder;


#[entry]
fn main() -> ! {
    let dp = stm32::Peripherals::take().unwrap();

    OverseerLED::initialize(dp.GPIOA, dp.GPIOB);
    CommandParser::initialize();
    dma::DmaSubsystem::initialize(dp.DMA2);

    unsafe { configure_bluetooth() };

    rotational_encoder::initialize(dp.TIM2, dp.TIM3, dp.EXTI);

    loop {}
}
