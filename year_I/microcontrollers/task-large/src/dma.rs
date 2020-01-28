use core::cell::{Cell, RefCell};
use stm32f4::stm32f411 as stm32;
use stm32::{interrupt};

use cortex_m::interrupt::{free, Mutex};

use heapless::String;
use heapless::spsc::{Queue};
use heapless::consts::{U256, U16};
use core::borrow::{BorrowMut, Borrow};
use core::ops::DerefMut;

use crate::command_parser::{CommandParser};

use crate::config;

static mut QUEUE: Queue<DmaRequest, U256, u8> = Queue(heapless::i::Queue::u8());

static DMA_SUBSYSTEM: Mutex<RefCell<Option< DmaSubsystem >>> =
    Mutex::new(RefCell::new(None));

static DMA_CHANNEL_WRITE_NUM: usize = 7;
static DMA_CHANNEL_READ_NUM: usize = 5;


pub(crate) struct DmaSubsystem {
    queue: &'static mut Queue<DmaRequest, U256, u8>,
    req_in_progress: Cell<Option<DmaRequest>>,
    dma2: stm32::DMA2,
    dma_immediate_buf: u8,
}

impl DmaSubsystem {

    pub(crate) fn initialize(dma2: stm32::DMA2) {

        free(|cs| {
            let dma = DmaSubsystem {
                queue: unsafe { &mut QUEUE },
                req_in_progress: Cell::new(None),
                dma2,
                dma_immediate_buf: 0,
            };

            DMA_SUBSYSTEM.borrow(cs).replace(Some (dma) );
            unsafe {
                config::configure_dma();
            };

            if let Some(ref mut dma) =  DMA_SUBSYSTEM.borrow(cs).borrow_mut().deref_mut() {
                dma.initiate_dma_read();
            }
        });
    }

    pub(crate) fn put_dma_request(req: DmaRequest) {
        free (|cs| {
            if let Some(ref mut dma) =  DMA_SUBSYSTEM.borrow(cs).borrow_mut().deref_mut() {
                match dma.queue.enqueue(req) {
                    Ok(_) => (),
                    Err(_req) => panic!("Dma queue is full")
                }

                if dma.dma2.st[DMA_CHANNEL_WRITE_NUM].cr.read().en().bit_is_clear() &&
                   dma.dma2.hisr.read().tcif7().bit_is_clear() {
                    dma.process_dma_queue();
                }
            }
        })
    }

    fn process_dma_queue(&mut self) {
        if let Some(req) = self.queue.borrow_mut().dequeue() {
            self.req_in_progress.replace(Some(req));
            self.start_dma_transfer();
        } else {
            self.req_in_progress.replace(None);
        }
    }

    fn start_dma_transfer(&self) {
        if let Some(r) = self.req_in_progress.take() {
            self.dma2.st[DMA_CHANNEL_WRITE_NUM].m0ar.write(|w| w.m0a().bits(
                r.data.as_ptr() as u32)
            );
            self.dma2.st[DMA_CHANNEL_WRITE_NUM].ndtr.write(|w| w.ndt().bits(r.data.len() as u16));
            self.dma2.st[DMA_CHANNEL_WRITE_NUM].cr.modify(|_, w| w.en().enabled());
        }
    }

    fn initiate_dma_read(&self) {
        self.dma2.st[DMA_CHANNEL_READ_NUM].m0ar.write(|w| w.m0a().bits(
            self.dma_immediate_buf.borrow() as *const u8 as u32)
        );
        self.dma2.st[DMA_CHANNEL_READ_NUM].ndtr.write(|w| w.ndt().bits(1));
        self.dma2.st[DMA_CHANNEL_READ_NUM].cr.modify(|_, w| w.en().enabled());
    }
}


pub(crate) struct DmaRequest {
    data: String<U16>
}

impl DmaRequest {
    pub(crate) fn build(s: String<U16>) -> DmaRequest {
        DmaRequest {data: s}
    }
}

#[interrupt]
fn DMA2_STREAM7() {
    free(|cs| {
        if let Some(ref mut dma) =  DMA_SUBSYSTEM.borrow(cs).borrow_mut().deref_mut() {

            if dma.dma2.hisr.read().tcif7().bit_is_set() {
                dma.dma2.hifcr.write(|w| w.ctcif7().set_bit());

                dma.process_dma_queue();
            }
        }
    });
}

#[interrupt]
fn DMA2_STREAM5() {
    free(|cs| {
        if let Some(ref mut dma) =  DMA_SUBSYSTEM.borrow(cs).borrow_mut().deref_mut() {

            if dma.dma2.hisr.read().tcif5().bit_is_set() {
                CommandParser::advance(cs, dma.dma_immediate_buf);

                dma.dma2.hifcr.write(|w| w.ctcif5().set_bit());

                dma.initiate_dma_read();
            }
        }
    });
}

