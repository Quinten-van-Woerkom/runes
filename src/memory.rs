/**
 * shared_memory.rs
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

use async_trait::async_trait;
use crate::clock;

/**
 * All shared memory regions must implement this shared memory trait, to
 * synchronize memory accesses. This allows us to create synchronization
 * boundaries only when shared memory is accessed, to minimize the
 * synchronization overhead.
 * 
 * Read-only memory must also support calls to write(), but is allowed to
 * implement it as a no-op.
 */
#[async_trait]
trait SharedMemory {
    async fn read(&self, address: u16, cycle: usize, clock: &clock::SystemClock) -> u8;
    async fn write(&self, address: u16, byte: u8, cycle: usize, clock: &clock::SystemClock);
}


/**
 * All shared memory on an NES system is owned and regulated by this Memory
 * object. All members of this struct implement SharedMemory and regulate their
 * own separate clock requirements.
 */
pub struct Memory {
    ppu: PpuRegisters,
    apu: ApuRegisters,
    cartridge: Cartridge,
}

impl Memory {
    pub fn empty() -> Self {
        unimplemented!()
    }
    
    pub fn from_rom(rom: std::fs::File) -> Self {
        unimplemented!()
    }

    pub fn from_save(rom: std::fs::File, save: std::fs::File) -> Self {
        unimplemented!()
    }
}


/**
 * To be implemented.
 */
struct PpuRegisters {}
struct ApuRegisters {}
struct Cartridge {}