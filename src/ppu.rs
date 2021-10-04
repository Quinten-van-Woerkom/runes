/**
 * ppu.rs
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

use crate::bitwise::Bitwise;
use crate::clock::Clock;
use crate::memory::StaticMemory;
use crate::yields::yields;

use std::cell::Cell;

/**
 * Any memory interface used with the NES pixel processing unit must implement
 * the following trait for interaction between the PPU and the "outside world".
 * 
 * Conceptually, it represents the PPU's pinout.
 */
pub trait Pinout {
    fn read(&self, address: u16, time: &Clock) -> Option<u8>;
    fn write(&self, address: u16, data: u8, time: &Clock) -> Option<()>;
    fn trigger_nmi(&self, time: &Clock) -> Option<()>;
    fn synchronize(&self, time: &Clock) -> Option<()>;
}

/**
 * The NES' pixel processing unit is a Ricoh 2C02.
 */
pub struct RicohRP2C02 {
    time: Clock,
    databus: Cell<u8>,
    vram_increment: Cell<u16>,
    base_nametable_address: Cell<u16>,
    read_buffer: Cell<u8>,

    pattern_tables: StaticMemory<0x0000, 0x2000>,
    palettes: StaticMemory<0x3f00, 0x0020>,

    address: Cell<u16>,
    vblank: Cell<bool>,
    sprite_overflow: Cell<bool>,
    sprite_zero_hit: Cell<bool>,

}

impl RicohRP2C02 {
    /**
     * Main loop of the PPU.
     */
    pub async fn run(&self, pinout: &impl Pinout) {
        loop {
            self.step(pinout).await;
        }
    }

    /**
     * Executes a single PPU cycle.
     */
    pub async fn step(&self, pinout: &impl Pinout) {
        self.tick();
        unimplemented!();
    }

    /**
     * Reads from one of the 8 CPU memory-mapped PPU registers.
     * Returns None if the PPU has not yet been emulated far enough in the
     * future to allow the operation, to enable support for asynchronous usage.
     */
    pub fn read_register(&self, pinout: &impl Pinout, address: u16, time: &Clock) -> Option<u8> {
        assert!(address >= 0x2000 && address <= 0x2007);
        if time > &self.time {
            None
        } else {
            Some(match address {
                0x2000 => self.databus.get(), //PPUCTRL
                0x2001 => self.databus.get(), //PPUMASK
                0x2002 => self.status(), //PPUSTATUS
                0x2003 => self.databus.get(), //OAMADDR
                0x2004 => unimplemented!(), //OAMDATA
                0x2005 => self.databus.get(), //PPUSCROLL
                0x2006 => self.databus.get(), //PPUADDR
                0x2007 => self.read_ppudata(pinout, address)?, //PPUDATA
                _ => unreachable!(),
            })
        }
    }

    /**
     * Writes to one of the 8 CPU memory-mapped PPU registers.
     * Returns None if the PPU has not yet been emulated far enough in the
     * future to allow the operation, to enable support for asynchronous usage.
     */
    pub fn write_register(&self, pinout: &impl Pinout, address: u16, data: u8, time: &Clock) -> Option<()> {
        if time > &self.time {
            None
        } else {
            self.databus.set(data);
            Some(match address {
                0x2000 => self.write_ppuctrl(data), //PPUCTRL
                0x2001 => unimplemented!(), //PPUMASK
                0x2002 => {}, //PPUSTATUS
                0x2003 => unimplemented!(), //OAMADDR
                0x2004 => unimplemented!(), //OAMDATA
                0x2005 => unimplemented!(), //PPUSCROLL
                0x2006 => unimplemented!(), //PPUADDR
                0x2007 => unimplemented!(), //PPUDATA
                _ => unreachable!(),
            })
        }
    }

    /**
     * Since there are quite some internal PPU registers that can be written to
     * from outside, we synchronize the PPU with the rest of the NES anytime
     * one of those is accessed.
     * NOTE: It might be useful, if this function needs to be called too often,
     * to make the PPU a "slave" of the CPU again, running for 3 ticks every
     * time the CPU ticks.
     */
    async fn synchronize(&self, pinout: &impl Pinout) {
        loop {
            match pinout.synchronize(&self.time) {
                None => yields().await,
                Some(()) => return,
            }
        }
    }

    /**
     * A single PPU cycle equals 4 master clock cycles.
     */
    fn tick(&self) {
        self.time.advance(4);
    }

    /**
     * Executes a read from VRAM. Can return None if the memory to be read from
     * has not yet been emulated sufficiently far.
     */
    fn read(&self, pinout: &impl Pinout, address: u16) -> Option<u8> {
        assert!(address < 0x4000);
        match address {
            0x0000..=0x1fff => Some(self.pattern_tables.read(address)),
            0x2000..=0x3eff => pinout.read(address, &self.time),
            0x3f00..=0x3fff => Some(self.palettes.read(address)),
            _ => unreachable!(),
        }
    }

    /**
     * Executes a write to VRAM. Can return None if the memory to be written to
     * has not yet been emulated sufficiently far.
     */
    fn write(&self, pinout: &impl Pinout, address: u16, data: u8) -> Option<()> {
        assert!(address < 0x4000);
        match address {
            0x0000..=0x1fff => self.pattern_tables.write(address, data),
            0x2000..=0x3eff => pinout.write(address, data, &self.time)?,
            0x3f00..=0x3fff => self.palettes.write(address, data),
            _ => unreachable!(),
        }
        Some(())
    }

    /**
     * Returns the value of the status register in byte form.
     * Clears the VBLANK bit as well (!).
     */
    fn status(&self) -> u8 {
        let result = (self.databus.get() as u8)
        | (self.sprite_overflow.get() as u8) << 5
        | (self.sprite_zero_hit.get() as u8) << 6
        | (self.vblank.get() as u8) << 7;
        self.vblank.set(false);
        result
    }

    /**
     * Executes the logic behind a read from the PPUDATA register. Note that
     * it can return None if trying to read from nametables when the cartridge
     * has not been emulated sufficiently far into the future.
     */
    fn read_ppudata(&self, pinout: &impl Pinout, address: u16) -> Option<u8> {
        let read_buffer = self.read_buffer.get();
        let buffer_address = 0x2000 | (address & 0x1fff);
        self.read_buffer.set(self.read(pinout, buffer_address)?);
        self.address.set(address + self.vram_increment.get());

        Some(match address % 0x4000 {
            0x0000..=0x3eff => read_buffer,
            0x3f00..=0x3fff => self.palettes.read(address),
            _ => unreachable!(),
        })
    }

    /**
     * Emulates a write to the PPUCTRL register.
     */
    fn write_ppuctrl(&self, data: u8) {
        self.base_nametable_address.set(match data.bits(0, 2) {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2c00,
            _ => unreachable!(),
        });

        self.vram_increment.set(match data.bit(2) {
            false => 1,
            true => 32,
        });


    }
}