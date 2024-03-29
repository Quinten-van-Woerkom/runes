/**
 * bus.rs
 * Part of Rust Nintendo Entertainment System emulator ("RuNES")
 * 
 * Copyright (c) 2021 Quinten van Woerkom
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

use crate::cartridge::{ Cartridge, load_cartridge };
use crate::clock::Clock;
use crate::cpu;

/**
 * All shared memory on an NES system is accessed through the bus, naturally.
 * This means that it also makes for a good synchronization barrier when
 * accessing this memory. Indeed, the memory keeps track of each processor's
 * clock to synchronize timing.
 * 
 * Keeping track of processor clocks is done by updating each clock whenever
 * the corresponding device accesses memory. This is chosen over keeping
 * references because of the ensuing lifetime issues and to prevent cacheline
 * contention (if we ever go parallel), since the clocks are updated frequently.
 * 
 * In a small deviation from reality, shared memory must also be accessed
 * through the bus, even when, in reality, it belongs to the accessing device,
 * like the PPU registers. This is needed to be able to force synchronization.
 */
pub struct Bus {
    cartridge: Box<dyn Cartridge>,
}

impl Bus {
    /**
     * Constructs a bus by loading a ROM file into memory.
     * Can fail if file I/O fails or if an incorrect ROM file is passed.
     */
    pub fn from_rom(path: &str) -> Result<Self, std::io::Error> {
        Ok(Self {
            // Safe because u8 and Cell<u8> have the same memory layout.
            cartridge: load_cartridge(path)?,
        })
    }
}

/**
 * Bus must be accessible for the CPU, which is achieved by emulating its
 * pinout interface.
 * For each CPU memory access, the memory's "copy" of the CPU clock is updated.
 */
impl cpu::Pinout for Bus {
    fn read(&self, address: u16, time: &Clock) -> Option<u8> {
        match address {
            0x4020..=0xffff => self.cartridge.cpu_read(address, time),
            // TODO: For now, returns 0
            _ => Some(0)
        }
    }

    fn write(&self, address: u16, data: u8, time: &Clock) -> Option<()> {
        match address {
            0x4020..=0xffff => self.cartridge.cpu_write(address, data, time),
            // TODO: For now, no-op
            _ => Some(())
        }
    }

    /**
     * Not yet emulated. For now, updates the clock and acts as if no interrupt
     * was raised.
     */
    fn nmi(&self, _time: &Clock) -> Option<bool> {
        Some(false)
    }

    /**
     * Not yet emulated. For now, updates the clock and acts as if no interrupt
     * was raised.
     */
    fn irq(&self, _time: &Clock) -> Option<bool> {
        Some(false)
    }

    /**
     * Not yet emulated. For now, updates the clock and acts as if no interrupt
     * was raised.
     */
    fn reset(&self, _time: &Clock) -> Option<bool> {
        Some(false)
    }
}

#[cfg(test)]
mod access {
    use super::*;

    /**
     * With the cycles and bytes for the CPU validated, we now check for actual
     * behaviour-wise correctness using nestest.
     */
    #[test]
    fn nestest() {
        use std::fs::File;
        use std::io::*;

        let bus = Bus::from_rom("nestest.nes").expect("Unable to load nestest rom");
        let cpu = cpu::Ricoh2A03::new();
        let nintendulator = BufReader::new(File::open("nestest.log").expect("Unable to load nestest log"));

        let mut history = Vec::new();
        for _ in 0..50 {
            history.push((cpu::Ricoh2A03::new(), String::new()));
        }

        for (index, log_line) in nintendulator.lines().enumerate() {
            let log_line = log_line.expect("Error reading Nintendulator log line");
            let nintendulator = cpu::Ricoh2A03::from_nintendulator(&log_line);
            history[index % 50] = (cpu.clone(), log_line);

            if cpu != nintendulator {
                if index >= 50 {
                    history.rotate_left(index % 50 + 1);
                }

                assert_eq!(cpu, nintendulator,
                    "\nHistory:\n{:?}
                    \nDoes not match Nintendulator log:\
                    {:?} (Current)\
                    {:?} (Correct)\n",
                    &history[0..index.min(50)],
                    cpu,
                    nintendulator
                );
            }

            futures::executor::block_on(cpu.step(&bus));
        }

        {
            use crate::cpu::Pinout;
            assert_eq!(bus.read(0x0002, &Clock::new()), Some(0x00), "Nestest failed: byte at $02 not $00, documented opcodes wrong");
            assert_eq!(bus.read(0x0003, &Clock::new()), Some(0x00), "Nestest failed: byte at $03 not $00, illegal opcodes wrong");
        }
    }
}