/**
 * clock.rs
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

use std::cell::Cell;

/**
 * Inter-device synchronization is achieved through separate clocks, each
 * keeping track of the emulated number of master clock cycles that have
 * elapsed at a moment in time.
 * Cycle counts for an NTSC NES are:
 *  12 master clock cycles per CPU cycle
 *  4 master clock cycles per PPU cycle
 */
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clock {
    cycle: Cell<u64>,
}

impl Clock {
    pub fn new() -> Self {
        Self { cycle: Cell::new(0) }
    }

    pub fn from(cycle: u64) -> Self {
        Self { cycle: Cell::from(cycle) }
    }

    pub fn advance(&self, cycles: u64) {
        self.cycle.set(self.cycle.get() + cycles);
    }

    pub fn current(&self) -> u64 {
        self.cycle.get()
    }
}