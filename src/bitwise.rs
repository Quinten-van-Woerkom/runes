/**
 * byte.rs
 * Part of Rust Nintendo Entertainment System emulator ("RuNES")
 * 
 * Implements convenience functions to help with common bit manipulations that
 * occur within the NES and emulation thereof.
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

use std::convert::TryInto;

/**
 * Both in NES functioning and emulation (e.g. .nes file type) a lot of bit
 * twiddling is required, so it is convenient to be able to access bits and
 * ranges of bits easily.
 */
pub trait Bitwise {
    fn bit(&self, index: usize) -> bool;
    fn bits(&self, index: usize, size: usize) -> Self;
    fn change_bit(&mut self, index: usize, value: bool);

    fn set_bit(&mut self, index: usize) {
        self.change_bit(index, true);
    }

    fn clear_bit(&mut self, index: usize) {
        self.change_bit(index, false);
    }
}

/**
 * Both u8 and u16 support bitwise operations.
 */
impl Bitwise for u8 {
    fn bit(&self, index: usize) -> bool {
        ((self >> index) & 1) != 0
    }

    fn bits(&self, index: usize, size: usize) -> Self {
        let mask = !(Self::MAX << size);
        (self >> index) & mask
    }

    fn change_bit(&mut self, index: usize, value: bool) {
        *self &= !((1 as Self) << index);
        *self |= (value as Self) << index;
    }
}

impl Bitwise for u16 {
    fn bit(&self, index: usize) -> bool {
        ((self >> index) & 1) != 0
    }

    fn bits(&self, index: usize, size: usize) -> Self {
        let mask = !(Self::MAX << size);
        (self >> index) & mask
    }

    fn change_bit(&mut self, index: usize, value: bool) {
        *self &= !((1 as Self) << index);
        *self |= (value as Self) << index;
    }
}

#[cfg(test)]
mod bitwise {
    use super::*;

    #[test]
    fn bit() {
        let ones = 0b11111111u8;
        let zeros = 0b00000000u8;
        let even = 0b01010101u8;
        let upper = 0b11110000u8;
        let lower = 0b00001111u8;
        let mut change = 0u8;

        for n in 0..8 {
            assert_eq!(ones.bit(n), true);
            assert_eq!(zeros.bit(n), false);
            assert_eq!(even.bit(n), n % 2 != 1);
            assert_eq!(upper.bit(n), n >= 4);
            assert_eq!(lower.bit(n), n < 4);

            assert_eq!(change.bit(n), false);
            change.set_bit(n);
            assert_eq!(change.bit(n), true);
            change.clear_bit(n);
            assert_eq!(change.bit(n), false);
            change.change_bit(n, true);
            assert_eq!(change.bit(n), true);
            change.change_bit(n, false);
            assert_eq!(change.bit(n), false);
        }
    }

    #[test]
    fn bits() {
        let ones = 0b11111111u8;
        let zeros = 0b00000000u8;
        let even = 0b01010101u8;
        let upper = 0b11110000u8;
        let lower = 0b00001111u8;

        assert_eq!(ones.bits(5, 3), 0b111u8);
        assert_eq!(zeros.bits(5, 3), 0b000u8);
        assert_eq!(even.bits(0, 4), 0b0101u8);
        assert_eq!(even.bits(4, 4), 0b0101u8);
        assert_eq!(upper.bits(0, 6), 0b110000u8);
        assert_eq!(lower.bits(0, 6), 0b001111u8);
    }
}


/**
 * Convenience functions to create and "dissect" two-byte words.
 */
pub trait Word {
    fn low_byte(&self) -> u8;
    fn high_byte(&self) -> u8;
    fn from_bytes(low_byte: u8, high_byte: u8) -> Self;
}

impl Word for u16 {
    fn low_byte(&self) -> u8 {
        (self & (u8::MAX as u16)).try_into().unwrap()
    }

    fn high_byte(&self) -> u8 {
        ((self >> 8) & (u8::MAX as u16)).try_into().unwrap()
    }

    fn from_bytes(low_byte: u8, high_byte: u8) -> Self {
        low_byte as u16 | ((high_byte as u16) << 8)
    }
}

#[cfg(test)]
mod word {
    use super::*;

    #[test]
    fn to_bytes() {
        assert_eq!(0xffff.low_byte(), 0xff);
        assert_eq!(0xffff.high_byte(), 0xff);
        assert_eq!(0xff00.low_byte(), 0x00);
        assert_eq!(0x00ff.low_byte(), 0xff);
        assert_eq!(0xff00.high_byte(), 0xff);
        assert_eq!(0x00ff.high_byte(), 0x00);
    }

    #[test]
    fn from_bytes() {
        assert_eq!(u16::from_bytes(0xae, 0xff), 0xffae);
        assert_eq!(u16::from_bytes(0x00, 0x00), 0x0000);
        assert_eq!(u16::from_bytes(0xff, 0xff), 0xffff);
    }
}