/**
 * memory.rs
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
use crate::cpu;
use crate::clock::Clock;
use std::cell::Cell;

/**
 * All shared memory on an NES system is accessed through the bus, naturally.
 * This means that it also makes for a good synchronization barrier when
 * accessing this memory.
 * 
 * In a small deviation from reality, shared memory must also be accessed
 * through the bus, even when, in reality, it belongs to the accessing device,
 * like the PPU registers. This is needed to be able to force synchronization.
 */
pub struct Memory {
    ram: [Cell<u8>; 0x800],
    cartridge: Box<dyn Cartridge>,
}

impl Memory {
    /**
     * Constructs a bus by loading a ROM file into memory.
     * Can fail if file I/O fails or if an incorrect ROM file is passed.
     */
    pub fn from_rom(path: &str) -> Result<Self, std::io::Error> {
        Ok(Self {
            // Safe because u8 and Cell<u8> have the same memory layout.
            ram: unsafe { std::mem::transmute::<[u8; 0x800], [Cell<u8>; 0x800]>([0u8; 0x800])},
            cartridge: load_cartridge(path)?,
        })
    }
}

impl cpu::Pinout for Memory {
    fn read(&self, address: u16, clock: &Clock) -> Option<u8> {
        match address {
            0x0000..=0x1fff => Some(self.ram[(address % 0x800) as usize].get()),
            0x4020..=0xffff => self.cartridge.cpu_read(address, clock),
            // TODO: For now, returns 0
            _ => Some(0)
        }
    }

    fn write(&self, address: u16, data: u8, clock: &Clock) -> Option<()> {
        match address {
            0x0000..=0x1fff => Some(self.ram[(address % 0x800) as usize].set(data)),
            0x4020..=0xffff => self.cartridge.cpu_write(address, data, clock),
            // TODO: For now, no-op
            _ => Some(())
        }
    }

    fn nmi(&self, time: &Clock) -> Option<bool> {
        None
    }

    fn irq(&self, time: &Clock) -> Option<bool> {
        None
    }

    fn reset(&self, time: &Clock) -> Option<bool> {
        None
    }
}

#[cfg(test)]
mod access {
    use super::*;

    /**
     * Since we already validate the CPU's instruction set correctness using a
     * dummy implementation of a bus, we can also validate the bus' correctness
     * by running nestest again, but then with the actual bus implementation
     * itself.
     */
    #[test]
    fn nestest() {
        use std::fs::File;
        use std::io::*;

        let bus = Memory::from_rom("nestest.nes").expect("Unable to load nestest rom");
        let mut cpu = cpu::Ricoh2A03::new();
        let nintendulator = BufReader::new(File::open("nestest.log").expect("Unable to load nestest log"));

        let mut history = Vec::new();
        for _ in 0..50 {
            history.push((cpu::Ricoh2A03::new(), String::new()));
        }

        for log_line in nintendulator.lines() {
            let log_line = log_line.expect("Error reading Nintendulator log line");
            let nintendulator = cpu::Ricoh2A03::from_nintendulator(&log_line);

            // Terribly inefficient, but fine, it's the easiest way to show
            // the execution history in order.
            history[0] = (cpu.clone(), log_line.clone());
            history.sort_by(|a, b| a.0.cycle().cmp(&b.0.cycle()));
            
            assert_eq!(cpu, nintendulator,
                "\nHistory:\n{:?}
                \nDoes not match Nintendulator log:\
                {:?} (Current)\
                {:?} (Correct)\n",
                history,
                cpu,
                nintendulator
            );

            futures::executor::block_on(cpu.step(&bus));
        }

        {
            use crate::cpu::Pinout;
            assert_eq!(bus.read(0x0002, &Clock::from(cpu.cycle())), Some(0x00), "Nestest failed: byte at $02 not $00, documented opcodes wrong");
            assert_eq!(bus.read(0x0003, &Clock::from(cpu.cycle())), Some(0x00), "Nestest failed: byte at $03 not $00, illegal opcodes wrong");
        }
    }
}