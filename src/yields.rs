/**
 * yield.rs
 * Part of Rust Nintendo Entertainment System emulator ("RuNES")
 * 
 * Because of timing interdependencies between processors like the CPU, PPU,
 * and APU, they must have some kind of synchronization among them. This is
 * achieved by a state machine created using Rust's async/await machinery, as
 * this allows for a quite natural workflow.
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
use std::task::{ Context, Poll };

/**
 * Yields directly, allowing other tasks to be run by the executor before
 * rescheduling the current task.
 */
pub async fn yields() {
    Yields{ polled: false }.await
}

struct Yields {
    polled: bool,
}

impl Future for Yields {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        if self.polled {
            Poll::Ready(())
        } else {
            self.polled = true;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

#[cfg(test)]
mod yields {
    use super::*;

    #[test]
    fn awaits_once() {
        use std::cell::Cell;

        async fn sets(set: &Cell<bool>) {
            set.set(true);
        }

        async fn checks(set: &Cell<bool>) {
            let mut counter = 0;
            while !set.get() {
                yields().await;
                counter += 1;
            }
            assert!(set.get());
            assert!(counter <= 1);
        }

        let set = Cell::new(false);
        let joined = async { futures::join!(checks(&set), sets(&set)); };

        futures::executor::block_on(joined);
    }
}