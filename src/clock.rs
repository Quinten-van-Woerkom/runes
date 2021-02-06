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

/**
 * Each device that accesses shared memory must explicitly keep track of its
 * current cycle, to allow cycle-accurate memory access synchronization.
 * For now, cycles are tracked in units of a single PPU cycle, as it is the
 * smallest time unit encountered.
 */
struct Clock {
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
    pub fn reset(&self, cycles: usize){
        let before = self.current.fetch_sub(cycles, Ordering::AcqRel);
        assert!(before >= cycles);
    }
}


#[cfg(test)]
mod test {
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