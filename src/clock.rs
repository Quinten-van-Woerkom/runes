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

use std::sync::atomic::{ AtomicUsize, Ordering };
use std::iter::Iterator;

/**
 * Each device that accesses shared memory must explicitly keep track of its
 * current cycle, to allow cycle-accurate memory access synchronization.
 * For now, cycles are tracked in units of a single PPU cycle, as it is the
 * smallest time unit encountered.
 */
pub struct Clock {
    current: AtomicUsize,
}

impl Clock {
    pub fn new() -> Self {
        Self {
            current: AtomicUsize::new(0)
        }
    }

    /**
     * Returns the current clock cycle.
     * Note: due to the monotonic nature of time, Ordering::Relaxed might be
     * possible. TODO: Test this.
     */
    pub fn current(&self) -> usize {
        self.current.load(Ordering::Acquire)
    }

    /**
     * Advances the clock by a given number of cycles.
     */
    pub fn advance(&self, cycles: usize) {
        self.current.fetch_add(cycles, Ordering::Release);
    }

    /**
     * Resets the cycle count by reducing its value. If clocks are initialized
     * at the same time and reset at equal moments with equal dislocations,
     * relative consistency is guaranteed.
     * Precondition: current must be bigger than offset cycles before reset.
     */
    fn reset(&self, cycles: usize){
        let before = self.current.fetch_sub(cycles, Ordering::AcqRel);
        assert!(before >= cycles);
    }
}


/**
 * The system clock keeps track of each synchronized component individually,
 * and ensures that their relative counts remain correct.
 * 
 * These four devices are the natural logical threads into which the NES is
 * divided. They operate mostly independently, but share data at a couple of
 * well-defined interfaces. These clock timings are used for synchronization
 * when any of these boundaries must be crossed.
 * 
 * TODO: Due to cacheline contention, it might be useful to add padding.
 */
pub struct SystemClock {
    pub cpu: Clock,
    pub ppu: Clock,
    pub apu: Clock,
    pub cartridge: Clock,
}

impl SystemClock {
    pub fn new() -> Self {
        Self {
            cpu: Clock::new(),
            ppu: Clock::new(),
            apu: Clock::new(),
            cartridge: Clock::new(),
        }
    }

    /**
     * To prevent overflow, so as to ensure coherence of relative times, we
     * must reset all clocks by reducing them with the cycle count of the
     * least-advanced clock.
     * 
     * It is important that this is done sometime before any clock overflows.
     * Keeping track of this is the responsibility of the user.
     */
    pub fn reset(&self) {
        let min = *[self.cpu.current(), self.ppu.current(), self.apu.current(), self.cartridge.current()].iter().min().unwrap();
        self.cpu.reset(min);
        self.ppu.reset(min);
        self.apu.reset(min);
        self.cartridge.reset(min);
    }
}


#[cfg(test)]
mod clock {
    use super::*;

    #[test]
    fn initialization() {
        let clock = Clock::new();
        assert_eq!(clock.current(), 0);
    }

    #[test]
    fn count() {
        let clock = Clock::new();
        clock.advance(3);
        assert_eq!(clock.current(), 3);
    }

    #[test]
    fn addition() {
        let clock = Clock::new();
        clock.advance(3);
        clock.advance(2);
        assert_eq!(clock.current(), 5);
    }

    #[test]
    fn persistence() {
        let clock = Clock::new();
        clock.advance(1);
        assert_eq!(clock.current(), 1);

        clock.advance(3);
        assert_eq!(clock.current(), 4);
    }

    #[test]
    fn reset() {
        let clock = Clock::new();
        clock.advance(300);
        clock.reset(299);
        assert_eq!(clock.current(), 1);
    }

    #[test]
    #[should_panic]
    fn invalid_reset() {
        let clock = Clock::new();
        clock.advance(300);
        clock.reset(301);
    }
}


#[cfg(test)]
mod system_clock {
    use super::*;

    #[test]
    fn initialization() {
        let system_clock = SystemClock::new();
        assert_eq!(system_clock.cpu.current(), 0);
        assert_eq!(system_clock.ppu.current(), 0);
        assert_eq!(system_clock.apu.current(), 0);
        assert_eq!(system_clock.cartridge.current(), 0);
    }

    #[test]
    fn coherence() {
        let system_clock = SystemClock::new();
        system_clock.cpu.advance(3);
        system_clock.reset();
        assert_eq!(system_clock.cpu.current(), 3);
        assert_eq!(system_clock.apu.current(), 0);

        system_clock.apu.advance(4);
        system_clock.ppu.advance(4);
        system_clock.cartridge.advance(4);
        system_clock.reset();
        assert_eq!(system_clock.cpu.current(), 0);
        assert_eq!(system_clock.apu.current(), 1);
        assert_eq!(system_clock.ppu.current(), 1);
        assert_eq!(system_clock.cartridge.current(), 1);
    }
}