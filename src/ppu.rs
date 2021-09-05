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

/**
 * Any memory interface used with the NES pixel processing unit must implement
 * the following trait for interaction between the PPU and the "outside world".
 * 
 * Conceptually, it represents the PPU's pinout.
 */
pub trait Pinout {
    fn read(&self, address: u16, time: &Clock) -> Option<u8>;
    fn write(&self, address: u16, data: u8, time: &Clock) -> Option<()>;
    fn int(&self, time: &Clock) -> Option<()>; // Triggers an NMI
    fn oam(&self, index: usize, time: &Clock) -> Option<(u8, u8, u8, u8)>;
}

/**
 * The NES' pixel processing unit is a Ricoh 2C02.
 */
struct RicohRP2C02 {
    
}

impl RicohRP2C02 {
    fn read_register(&self, address: u16, time: &Clock) -> Option<u8> {
        unimplemented!()
    }

    fn write_register(&self, address: u16, data: u8, time: &Clock) -> Option<()> {
        unimplemented!()
    }
}