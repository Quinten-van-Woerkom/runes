mod bitwise;
mod bus;
mod cartridge;
mod clock;
mod cpu;
mod yields;

use std::cell::Cell;
use std::io::*;
use crate::clock::Clock;

/**
 * For testing purposes, we here use a bus that directly writes to
 * an array, without memory mapping.
 */
struct ArrayBus {
    data: [Cell<u8>; 0x10000],
}

impl ArrayBus {
    fn new() -> Self {
        Self { data: unsafe { std::mem::transmute([0u8; 0x10000]) } }
    }

    fn load_nestest(path: &std::path::Path) -> std::io::Result<Self> {
        let mut buffer = [0u8; 0x10000];
        let mut file = std::fs::File::open(path)?;
        file.seek(SeekFrom::Start(16))?;
        file.read_exact(&mut buffer[0x8000..=0xbfff])?;

        for i in 0xc000..=0xffff {
            buffer[i] = buffer[i.wrapping_sub(0x4000)];
        }

        Ok(Self { data: unsafe { std::mem::transmute(buffer) } } )
    }
}

impl cpu::Bus for ArrayBus {
    fn read(&self, address: u16, _time: &Clock) -> Option<u8> {
        Some(self.data[address as usize].get())
    }

    fn write(&self, address: u16, data: u8, _time: &Clock) -> Option<()> {
        self.data[address as usize].set(data);
        Some(())
    }

    fn nmi(&self, _time: &Clock) -> Option<bool> { Some(false) }
    fn irq(&self, _time: &Clock) -> Option<bool> { Some(false) }
    fn reset(&self, _time: &Clock) -> Option<bool> { Some(false) }
}

fn main() {
    let mut cpu = cpu::Ricoh2A03::new();
    let bus = ArrayBus::load_nestest(std::path::Path::new("nestest.nes")).unwrap();
    futures::executor::block_on(cpu.run(&bus));
}
