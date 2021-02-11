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

use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{ AtomicU64, Ordering };
use std::task::{ Context, Poll };

/**
 * Each device that accesses shared memory must explicitly keep track of its
 * current cycle, to allow cycle-accurate memory access synchronization.
 * For now, cycles are tracked in units of a single PPU cycle, as it is the
 * smallest time unit encountered.
 */
pub struct Clock {
    current: AtomicU64,
}

impl Clock {
    pub fn new() -> Self {
        Self {
            current: AtomicU64::new(0)
        }
    }

    /**
     * Returns the current clock cycle.
     * Note: due to the monotonic nature of time, Ordering::Relaxed might be
     * possible. TODO: Test this.
     */
    pub fn current(&self) -> u64 {
        self.current.load(Ordering::Acquire)
    }

    /**
     * Advances the clock by a given number of cycles.
     */
    pub fn advance(&self, cycles: u64) {
        self.current.fetch_add(cycles, Ordering::Release);
    }

    /**
     * Awaits until the clock reaches a given time point.
     * Useful for synchronization requirements.
     */
    pub async fn await_until(&self, cycles: u64) {
        ClockSynchronization::new(&self, cycles).await;
    }
}


/**
 * Future that represents a clock synchronization, asynchronously waiting until
 * the given clock reaches a certain cycle count.
 */
struct ClockSynchronization<'a> {
    clock: &'a Clock,
    cycles: u64,
}

impl<'a> ClockSynchronization<'a> {
    pub fn new(clock: &'a Clock, cycles: u64) -> Self {
        Self {
            clock: clock,
            cycles: cycles,
        }
    }
}

impl<'a> Future for ClockSynchronization<'a> {
    type Output = ();

    fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        println!("Polling...");
        if self.clock.current() > self.cycles {
            println!("Ready!");
            Poll::Ready(())
        } else {
            // For now, we wake always, but it might be better to create a
            // custom executor that ignores a waker, and instead runs the
            // thread emulating the device that is farthest behind.
            context.waker().wake_by_ref();
            Poll::Pending
        }
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
}