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

use crate::cartridge;
use std::cell::Cell;

/**
 * All shared memory on an NES system is accessed through the bus, naturally.
 * This means that it also makes for a good synchronization barrier when
 * accessing this memory.
 * 
 * In a small deviation from reality, shared memory must also be accessed`
 * through the bus, even when, in reality, it belongs to the accessing device,
 * like the PPU registers. This is needed to be able to force synchronization.
 */
pub struct Bus {
    ram: [Cell<u8>; 0x800],
    ppu: PpuRegisters,
    apu: ApuRegisters,
    cartridge: Box<dyn cartridge::Cartridge>,
}

impl Bus {
    pub fn _empty() -> Self {
        unimplemented!()
    }
    
    pub fn _from_rom(_rom: std::fs::File) -> Self {
        unimplemented!()
    }

    pub fn _from_save(_rom: std::fs::File, _save: std::fs::File) -> Self {
        unimplemented!()
    }
}


/**
 * To be implemented.
 */
struct PpuRegisters {}
struct ApuRegisters {}