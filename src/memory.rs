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

use std::cell::Cell;

/**
 * A lot of addressable memory in the NES is simply static data that is
 * mirrored over a certain range of addresses.
 * For this type of memory, we here implement a generic type.
 */
#[derive(Clone)]
pub struct StaticMemory<const ADDRESS: usize, const SIZE: usize> {
    data: [Cell<u8>; SIZE],
}

impl<const ADDRESS: usize, const SIZE: usize> StaticMemory<ADDRESS, SIZE> {
    pub fn new() -> Self {
        Self {
            data: unsafe { [0u8; SIZE].as_ptr().cast::<[Cell<u8>; SIZE]>().read() },
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        let index = (address as usize - ADDRESS) % SIZE;
        self.data[index].get()
    }

    pub fn write(&self, address: u16, data: u8) {
        let index = (address as usize - ADDRESS) % SIZE;
        self.data[index].set(data);
    }
}

#[cfg(test)]
mod static_memory {
    use super::*;

    #[test]
    fn initialization() {
        let memory = StaticMemory::<0x2000, 0x0800>::new();

        for address in 0x2000..0x2800 {
            assert_eq!(memory.read(address), 0x0000);
        }
    }

    #[test]
    fn persistence() {
        let memory = StaticMemory::<0x2000, 0x0800>::new();

        for address in 0x2000..0x2800 {
            memory.write(address, 0xff);
        }

        for address in 0x2000..0x2800 {
            assert_eq!(memory.read(address), 0xff);
        }
    }

    #[test]
    fn mirroring() {
        use std::convert::TryInto;

        let memory = StaticMemory::<0x2000, 0x0800>::new();

        // Writing in "regular" memory and checking that mirrored memory reflects this.
        for address in 0x2000..0x2800 {
            memory.write(address, (address % 0xff).try_into().unwrap());
            assert_eq!(memory.read(address), memory.read(address + 0x0800));
        }

        // Writing in mirrored memory and checking that "regular" memory reflects this.
        for address in 0x2000..0x2800 {
            memory.write(address + 0x0800, (address % 0xff).try_into().unwrap());
            assert_eq!(memory.read(address), memory.read(address));
        }
    }
}