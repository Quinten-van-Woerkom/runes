/**
 * cpu.rs
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

use crate::bitwise::{ Bitwise, Word };
use crate::clock::Clock;
use crate::yields::yields;

use std::cell::Cell;

/**
 * Any address bus used with the Ricoh 2A03 must implement this trait to allow
 * for interoperability. Corresponds with the CPU pinout, conceptually.
 * 
 * Functions return an empty Option only when the addressed memory or device is
 * not yet ready, i.e. if some other device must be emulated further in time
 * first. It would be preferred to directly write them as async functions, but
 * that is not currently possible in Rust. The async-trait crate would be a
 * solution, but it has overhead in terms of memory allocations, which add up
 * to a significant amount when used for every read or write.
 */
pub trait Pinout {
    fn read(&self, address: u16, time: &Clock) -> Option<u8>;
    fn write(&self, address: u16, data: u8, time: &Clock) -> Option<()>;

    fn nmi(&self, time: &Clock) -> Option<bool>;
    fn irq(&self, time: &Clock) -> Option<bool>;
    fn reset(&self, time: &Clock) -> Option<bool>;
}

/**
 * Emulated internals of the Ricoh 2A03.
 */
pub struct Ricoh2A03 {
    time: Clock,
    status: Status,
    program_counter: Cell<u16>,
    stack_pointer: Cell<u8>,
    a: Cell<u8>,
    x: Cell<u8>,
    y: Cell<u8>,
    operand: Cell<u8>, // Data bus register, combination of SB/DB
    address: Cell<u16>, // Address bus register, combination of ADL/ADH/ABL/ABH
}

impl Ricoh2A03 {
    pub fn new() -> Self {
        Self {
            time: Clock::from(7), // Start-up takes 7 cycles
            status: Status::new(),
            program_counter: 0xc000.into(),
            stack_pointer: 0xfd.into(),
            a: 0x00.into(),
            x: 0x00.into(),
            y: 0x00.into(),
            operand: 0x00.into(),
            address: 0x0000.into(),
        }
    }

    /**
     * For testing purposes, we also allow a CPU to be constructed from a
     * Nintendulator log entry.
     */
    #[cfg(test)]
    pub fn from_nintendulator(log: &str) -> Self {
        let mut state = log.split_whitespace();
        let program_counter = u16::from_str_radix(state.next().unwrap(), 16).unwrap().into();
        let time = Clock::from(usize::from_str_radix(state.next_back().unwrap().strip_prefix("CYC:").unwrap(), 10).unwrap());

        let mut skip = state.next().unwrap().strip_prefix("A:");
        while skip.is_none() {
            skip = state.next().unwrap().strip_prefix("A:");
        }

        let a = u8::from_str_radix(skip.unwrap(), 16).unwrap().into();
        let x = u8::from_str_radix(state.next().unwrap().strip_prefix("X:").unwrap(), 16).unwrap().into();
        let y = u8::from_str_radix(state.next().unwrap().strip_prefix("Y:").unwrap(), 16).unwrap().into();
        let status = u8::from_str_radix(state.next().unwrap().strip_prefix("P:").unwrap(), 16).unwrap().into();
        let stack_pointer = u8::from_str_radix(state.next().unwrap().strip_prefix("SP:").unwrap(), 16).unwrap().into();

        Self {
            time,
            status,
            program_counter,
            stack_pointer,
            a,
            x,
            y,
            operand: 0x00.into(),
            address: 0x0000.into(),
        }
    }

    pub async fn run(&self, pinout: &impl Pinout) {
        loop {
            self.step(pinout).await;
        }
    }

    pub async fn step(&self, pinout: &impl Pinout) {
        let opcode = self.fetch(pinout).await;
        self.execute(pinout, opcode).await;
    }

    /**
     * Executes the instruction associated with the given opcode.
     * Implemented as a combination of macros for the addressing modes and
     * operation types.
     */
    async fn execute(&self, pinout: &impl Pinout, opcode: u8) {
        /**
         * All branch instructions follow a similar pattern, so we use a macro
         * to generate them.
         */
        macro_rules! branch {
            ($flag:ident, $value:expr) => {{
                self.relative(pinout).await;
                if self.status.$flag.get() == $value {
                    self.time.tick();
                    if self.address.get().high_byte() != self.program_counter.get().high_byte() {
                        self.time.tick();
                    }
                    self.program_counter.set(self.address.get());
                }
            }};
        }

        /**
         * Sets a given status flag.
         */
        macro_rules! set {
            ($flag:ident, $value:expr) => {{
                self.time.tick();
                self.status.$flag.set($value);
            }}
        }

        /**
         * Address-based instructions operate on an address only, but don't
         * read from the corresponding memory location.
         * NOP and JMP are special-case in that they are the only address-based
         * instructions that do not access the bus.
         */
        macro_rules! address {
            (nop, $addressing:ident) => {{
                self.$addressing(pinout).await;
                self.nop();
            }};

            (jmp, $addressing:ident) => {{
                self.$addressing(pinout).await;
                self.jmp();
            }};

            ($instruction:ident, $addressing:ident) => {{
                self.$addressing(pinout).await;
                self.$instruction(pinout).await;
            }}
        }

        /**
         * Read instructions store the result of their operation only within
         * the CPU itself and do not write it elsewhere.
         * Depending on the addressing mode, a "correction cycle" must be
         * applied sometimes, when a page crossing must be accounted for.
         */
        macro_rules! read {
            ($instruction:ident, $addressing:ident) => {{
                self.$addressing(pinout).await;
                self.operand.set(self.read(pinout).await);
                self.$instruction();
            }};
        }

        /**
         * Modify instructions write the result of an operation back to memory
         * in some way. This can be the memory bus, or the accumulator,
         * depending on the addressing mode.
         */
        macro_rules! modify {
            ($instruction:ident, accumulator) => {{
                self.operand.set(self.a.get());
                self.$instruction();
                self.a.set(self.operand.get());
            }};

            ($instruction:ident, $addressing:ident) => {{
                self.$addressing(pinout).await;
                self.operand.set(self.read(pinout).await);
                self.$instruction();
                self.write(pinout, self.operand.get()).await;
            }};
        }

        /**
         * Some undocumented opcodes are essentially combinations of two
         * documented instructions, one being read-modify-write, followed by
         * a read instruction.
         * TODO: Find out if indirect, y and absolute indexed should take an
         * extra cycle for page crosses or not. Nestest does not verify this.
         */
        macro_rules! combine {
            ($modify:ident, $read:ident, $addressing:ident) => {{
                self.$addressing(pinout).await;
                self.operand.set(self.read(pinout).await);
                self.$modify();
                self.$read();
                self.write(pinout, self.operand.get()).await;
            }}
        }

        /**
         * Implied instructions don't have a corresponding addressing mode.
         */
        macro_rules! implied {
            ($instruction:ident) => {{
                self.$instruction();
            }};
        }

        /**
         * System instructions are special in that they have an implied
         * addressing mode, but do access the bus directly.
         */
        macro_rules! system {
            ($instruction:ident) => {{
                self.$instruction(pinout).await;
            }}
        }

        match opcode {
            0x00 => system!(brk),
            0x01 => read!(ora, indirect_x),
            0x02 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x03 => combine!(asl, ora, indirect_x),
            0x04 => address!(nop, zeropage),
            0x05 => read!(ora, zeropage),
            0x06 => modify!(asl, zeropage),
            0x07 => combine!(asl, ora, zeropage),
            0x08 => system!(php),
            0x09 => read!(ora, immediate),
            0x0a => modify!(asl, accumulator),
            0x0b => read!(anc, immediate),
            0x0c => address!(nop, absolute),
            0x0d => read!(ora, absolute),
            0x0e => modify!(asl, absolute),
            0x0f => combine!(asl, ora, absolute),
            0x10 => branch!(negative, false),
            0x11 => read!(ora, indirect_y_read),
            0x12 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x13 => combine!(asl, ora, indirect_y_write),
            0x14 => address!(nop, zeropage_x),
            0x15 => read!(ora, zeropage_x),
            0x16 => modify!(asl, zeropage_x),
            0x17 => combine!(asl, ora, zeropage_x),
            0x18 => set!(carry, false),
            0x19 => read!(ora, absolute_y_read),
            0x1a => implied!(nop),
            0x1b => combine!(asl, ora, absolute_y_write),
            0x1c => address!(nop, absolute_x_read),
            0x1d => read!(ora, absolute_x_read),
            0x1e => modify!(asl, absolute_x_write),
            0x1f => combine!(asl, ora, absolute_x_write),
            0x20 => system!(jsr),
            0x21 => read!(and, indirect_x),
            0x22 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x23 => combine!(rol, and, indirect_x),
            0x24 => read!(bit, zeropage),
            0x25 => read!(and, zeropage),
            0x26 => modify!(rol, zeropage),
            0x27 => combine!(rol, and, zeropage),
            0x28 => system!(plp),
            0x29 => read!(and, immediate),
            0x2a => modify!(rol, accumulator),
            0x2b => read!(anc, immediate),
            0x2c => read!(bit, absolute),
            0x2d => read!(and, absolute),
            0x2e => modify!(rol, absolute),
            0x2f => combine!(rol, and, absolute),
            0x30 => branch!(negative, true),
            0x31 => read!(and, indirect_y_read),
            0x32 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x33 => combine!(rol, and, indirect_y_write),
            0x34 => address!(nop, zeropage_x),
            0x35 => read!(and, zeropage_x),
            0x36 => modify!(rol, zeropage_x),
            0x37 => combine!(rol, and, zeropage_x),
            0x38 => set!(carry, true),
            0x39 => read!(and, absolute_y_read),
            0x3a => implied!(nop),
            0x3b => combine!(rol, and, absolute_y_write),
            0x3c => address!(nop, absolute_x_read),
            0x3d => read!(and, absolute_x_read),
            0x3e => modify!(rol, absolute_x_write),
            0x3f => combine!(rol, and, absolute_x_write),
            0x40 => system!(rti),
            0x41 => read!(eor, indirect_x),
            0x42 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x43 => combine!(lsr, eor, indirect_x),
            0x44 => address!(nop, zeropage),
            0x45 => read!(eor, zeropage),
            0x46 => modify!(lsr, zeropage),
            0x47 => combine!(lsr, eor, zeropage),
            0x48 => system!(pha),
            0x49 => read!(eor, immediate),
            0x4a => modify!(lsr, accumulator),
            0x4b => read!(alr, immediate),
            0x4c => address!(jmp, absolute),
            0x4d => read!(eor, absolute),
            0x4e => modify!(lsr, absolute),
            0x4f => combine!(lsr, eor, absolute),
            0x50 => branch!(overflow, false),
            0x51 => read!(eor, indirect_y_read),
            0x52 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x53 => combine!(lsr, eor, indirect_y_write),
            0x54 => address!(nop, zeropage_x),
            0x55 => read!(eor, zeropage_x),
            0x56 => modify!(lsr, zeropage_x),
            0x57 => combine!(lsr, eor, zeropage_x),
            0x58 => set!(interrupt_disable, false),
            0x59 => read!(eor, absolute_y_read),
            0x5a => implied!(nop),
            0x5b => combine!(lsr, eor, absolute_y_write),
            0x5c => address!(nop, absolute_x_read),
            0x5d => read!(eor, absolute_x_read),
            0x5e => modify!(lsr, absolute_x_write),
            0x5f => combine!(lsr, eor, absolute_x_write), // Unsure if this should be cross or not
            0x60 => system!(rts),
            0x61 => read!(adc, indirect_x),
            0x62 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x63 => combine!(ror, adc, indirect_x),
            0x64 => address!(nop, zeropage),
            0x65 => read!(adc, zeropage),
            0x66 => modify!(ror, zeropage),
            0x67 => combine!(ror, adc, zeropage),
            0x68 => system!(pla),
            0x69 => read!(adc, immediate),
            0x6a => modify!(ror, accumulator),
            0x6b => read!(arr, immediate),
            0x6c => address!(jmp, indirect),
            0x6d => read!(adc, absolute),
            0x6e => modify!(ror, absolute),
            0x6f => combine!(ror, adc, absolute),
            0x70 => branch!(overflow, true),
            0x71 => read!(adc, indirect_y_read),
            0x72 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x73 => combine!(ror, adc, indirect_y_write),
            0x74 => address!(nop, zeropage_x),
            0x75 => read!(adc, zeropage_x),
            0x76 => modify!(ror, zeropage_x),
            0x77 => combine!(ror, adc, zeropage_x),
            0x78 => set!(interrupt_disable, true),
            0x79 => read!(adc, absolute_y_read),
            0x7a => implied!(nop),
            0x7b => combine!(ror, adc, absolute_y_write), // Unsure if this should be cross or not
            0x7c => address!(nop, absolute_x_read),
            0x7d => read!(adc, absolute_x_read),
            0x7e => modify!(ror, absolute_x_write),
            0x7f => combine!(ror, adc, absolute_x_write), // Unsure if this should be cross or not
            0x80 => address!(nop, immediate),
            0x81 => address!(sta, indirect_x),
            0x82 => address!(nop, immediate),
            0x83 => address!(sax, indirect_x),
            0x84 => address!(sty, zeropage),
            0x85 => address!(sta, zeropage),
            0x86 => address!(stx, zeropage),
            0x87 => address!(sax, zeropage),
            0x88 => implied!(dey),
            0x89 => address!(nop, immediate),
            0x8a => implied!(txa),
            0x8b => read!(xaa, immediate),
            0x8c => address!(sty, absolute),
            0x8d => address!(sta, absolute),
            0x8e => address!(stx, absolute),
            0x8f => address!(sax, absolute),
            0x90 => branch!(carry, false),
            0x91 => address!(sta, indirect_y_write),
            0x92 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x93 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x94 => address!(sty, zeropage_x),
            0x95 => address!(sta, zeropage_x),
            0x96 => address!(stx, zeropage_y),
            0x97 => address!(sax, zeropage_y),
            0x98 => implied!(tya),
            0x99 => address!(sta, absolute_y_write),
            0x9a => implied!(txs),
            0x9b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x9c => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x9d => address!(sta, absolute_x_write),
            0x9e => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x9f => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xa0 => read!(ldy, immediate),
            0xa1 => read!(lda, indirect_x),
            0xa2 => read!(ldx, immediate),
            0xa3 => read!(lax, indirect_x),
            0xa4 => read!(ldy, zeropage),
            0xa5 => read!(lda, zeropage),
            0xa6 => read!(ldx, zeropage),
            0xa7 => read!(lax, zeropage),
            0xa8 => implied!(tay),
            0xa9 => read!(lda, immediate),
            0xaa => implied!(tax),
            0xab => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xac => read!(ldy, absolute),
            0xad => read!(lda, absolute),
            0xae => read!(ldx, absolute),
            0xaf => read!(lax, absolute),
            0xb0 => branch!(carry, true),
            0xb1 => read!(lda, indirect_y_read),
            0xb2 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0xb3 => read!(lax, indirect_y_read),
            0xb4 => read!(ldy, zeropage_x),
            0xb5 => read!(lda, zeropage_x),
            0xb6 => read!(ldx, zeropage_y),
            0xb7 => read!(lax, zeropage_y),
            0xb8 => set!(overflow, false),
            0xb9 => read!(lda, absolute_y_read),
            0xba => implied!(tsx),
            0xbb => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xbc => read!(ldy, absolute_x_read),
            0xbd => read!(lda, absolute_x_read),
            0xbe => read!(ldx, absolute_y_read),
            0xbf => read!(lax, absolute_y_read),
            0xc0 => read!(cpy, immediate),
            0xc1 => read!(cmp, indirect_x),
            0xc2 => address!(nop, immediate),
            0xc3 => combine!(dec, cmp, indirect_x),
            0xc4 => read!(cpy, zeropage),
            0xc5 => read!(cmp, zeropage),
            0xc6 => modify!(dec, zeropage),
            0xc7 => combine!(dec, cmp, zeropage),
            0xc8 => implied!(iny),
            0xc9 => read!(cmp, immediate),
            0xca => implied!(dex),
            0xcb => read!(axs, immediate),
            0xcc => read!(cpy, absolute),
            0xcd => read!(cmp, absolute),
            0xce => modify!(dec, absolute),
            0xcf => combine!(dec, cmp, absolute),
            0xd0 => branch!(zero, false),
            0xd1 => read!(cmp, indirect_y_read),
            0xd2 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0xd3 => combine!(dec, cmp, indirect_y_write),
            0xd4 => address!(nop, zeropage_x),
            0xd5 => read!(cmp, zeropage_x),
            0xd6 => modify!(dec, zeropage_x),
            0xd7 => combine!(dec, cmp, zeropage_x),
            0xd8 => set!(decimal_mode, false),
            0xd9 => read!(cmp, absolute_y_read),
            0xda => implied!(nop),
            0xdb => combine!(dec, cmp, absolute_y_write), // Unsure if this should be cross or not
            0xdc => address!(nop, absolute_x_read),
            0xdd => read!(cmp, absolute_x_read),
            0xde => modify!(dec, absolute_x_write),
            0xdf => combine!(dec, cmp, absolute_x_write), // Unsure if this should be cross or not
            0xe0 => read!(cpx, immediate),
            0xe1 => read!(sbc, indirect_x),
            0xe2 => address!(nop, immediate),
            0xe3 => combine!(inc, sbc, indirect_x),
            0xe4 => read!(cpx, zeropage),
            0xe5 => read!(sbc, zeropage),
            0xe6 => modify!(inc, zeropage),
            0xe7 => combine!(inc, sbc, zeropage),
            0xe8 => implied!(inx),
            0xe9 => read!(sbc, immediate),
            0xea => implied!(nop),
            0xeb => read!(sbc, immediate),
            0xec => read!(cpx, absolute),
            0xed => read!(sbc, absolute),
            0xee => modify!(inc, absolute),
            0xef => combine!(inc, sbc, absolute),
            0xf0 => branch!(zero, true),
            0xf1 => read!(sbc, indirect_y_read),
            0xf2 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0xf3 => combine!(inc, sbc, indirect_y_write),
            0xf4 => address!(nop, zeropage_x),
            0xf5 => read!(sbc, zeropage_x),
            0xf6 => modify!(inc, zeropage_x),
            0xf7 => combine!(inc, sbc, zeropage_x),
            0xf8 => set!(decimal_mode, true),
            0xf9 => read!(sbc, absolute_y_read),
            0xfa => implied!(nop),
            0xfb => combine!(inc, sbc, absolute_y_write),
            0xfc => address!(nop, absolute_x_read),
            0xfd => read!(sbc, absolute_x_read),
            0xfe => modify!(inc, absolute_x_write),
            0xff => combine!(inc, sbc, absolute_x_write),
        }
    }

    /**
     * Reads from the bus, taking one cycle.
     */
    async fn read(&self, pinout: &impl Pinout) -> u8 {
        loop {
            match pinout.read(self.address.get(), &self.time) {
                None => yields().await,
                Some(data) => {
                    self.time.tick();
                    return data;
                },
            }
        }
    }

    /**
     * Writes to the bus, taking one cycle.
     */
    async fn write(&self, pinout: &impl Pinout, data: u8) {
        loop {
            match pinout.write(self.address.get(), data, &self.time) {
                None => yields().await,
                Some(()) => return self.time.tick(),
            }
        }
    }

    /**
     * Pushes a value onto the stack. Takes one cycle.
     */
    async fn push(&self, pinout: &impl Pinout, data: u8) {
        self.address.set(0x0100 | self.stack_pointer.get() as u16);
        self.write(pinout, data).await;
        self.stack_pointer.set(self.stack_pointer.get().wrapping_sub(1));
    }

    /**
     * Pulls a value from the stack. Takes two cycles, or one if the previous
     * operation was also a pull. As such, the second cycle must be added
     * manually.
     */
    async fn pull(&self, pinout: &impl Pinout) -> u8 {
        self.stack_pointer.set(self.stack_pointer.get().wrapping_add(1));
        self.address.set(0x0100 | self.stack_pointer.get() as u16);
        self.read(pinout).await
    }

    /**
     * Reads a byte from the location of the program counter, and increments
     * the program counter afterwards. Takes one cycle, because of the read.
     */
    async fn fetch(&self, pinout: &impl Pinout) -> u8 {
        self.address.set(self.program_counter.get());
        self.program_counter.set(self.program_counter.get().wrapping_add(1));
        self.read(pinout).await
    }

    /**
     * For some instructions, page boundary crossings incur an additional cycle
     * of overhead, because the page adjustment requires an extra operation.
     */
    fn adjust_page(&self, address: u16) {
        let page_crossing = address.high_byte() != self.address.get().high_byte();
        if page_crossing {
            self.time.tick();
        }
    }


    /**
     * Immediate addressing operates straight on the operand.
     */
    async fn immediate(&self, _pinout: &impl Pinout) {
        self.address.set(self.program_counter.get());
        self.program_counter.set(self.program_counter.get().wrapping_add(1));
    }

    /**
     * Zeropage addressing interprets the operand as the effective address.
     */
    async fn zeropage(&self, pinout: &impl Pinout) {
        self.address.set(self.fetch(pinout).await as u16);
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by
     * adding an index register to the immediate operand, and
     * interpreting that as an address.
     * Note: the high byte is always zero, page boundary crossings wrap
     * around instead.
     */
    async fn zeropage_x(&self, pinout: &impl Pinout) {
        self.address.set(self.fetch(pinout).await.wrapping_add(self.x.get()) as u16);
        self.time.tick();
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by
     * adding an index register to the immediate operand, and
     * interpreting that as an address.
     * Note: the high byte is always zero, page boundary crossings wrap
     * around instead.
     */
    async fn zeropage_y(&self, pinout: &impl Pinout) {
        self.address.set(self.fetch(pinout).await.wrapping_add(self.y.get()) as u16);
        self.time.tick();
    }

    /**
     * Relative addressing loads the program counter, offset by a given
     * amount.
     */
    async fn relative(&self, pinout: &impl Pinout) {
        let offset = self.fetch(pinout).await as i8;
        self.address.set(self.program_counter.get().wrapping_add(offset as u16));
    }

    /**
     * Absolute addressing loads the two bytes after the opcode into a
     * 16-bit word that it uses as effective address.
     */
    async fn absolute(&self, pinout: &impl Pinout) {
        let low_byte = self.fetch(pinout).await;
        let high_byte = self.fetch(pinout).await;
        self.address.set(u16::from_bytes(low_byte, high_byte));
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Read instructions using the absolute X addressing mode take an extra
     * cycle if the address increment crosses a page.
     */
    async fn absolute_x_read(&self, pinout: &impl Pinout) {
        self.absolute(pinout).await;
        let address = self.address.get().wrapping_add(self.x.get() as u16);
        self.adjust_page(address);
        self.address.set(address);
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Read instructions using the absolute Y addressing mode take an extra
     * cycle if the address increment crosses a page.
     */
    async fn absolute_y_read(&self, pinout: &impl Pinout) {
        self.absolute(pinout).await;
        let address = self.address.get().wrapping_add(self.y.get() as u16);
        self.adjust_page(address);
        self.address.set(address);
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Read instructions using the absolute X addressing mode take an extra
     * cycle if the address increment crosses a page.
     */
    async fn absolute_x_write(&self, pinout: &impl Pinout) {
        self.absolute(pinout).await;
        self.address.set(self.address.get().wrapping_add(self.x.get() as u16));
        self.time.tick();
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Write instructions using the absolute Y addressing mode always take an
     * extra cycle.
     */
    async fn absolute_y_write(&self, pinout: &impl Pinout) {
        self.absolute(pinout).await;
        self.address.set(self.address.get().wrapping_add(self.y.get() as u16));
        self.time.tick();
    }

    /**
     * Indirect addressing uses a 16-bit operand as address from which it reads
     * the actual address to operate on.
     * Is bugged in the sense that indirect vectors on a page boundary do not
     * correctly cross that page boundary, e.g. $00ff reads from ($00ff, $0000)
     * instead of ($00ff, $0100).
     */
    async fn indirect(&self, pinout: &impl Pinout) {
        self.absolute(pinout).await;
        let high_address = u16::from_bytes(
            self.address.get().low_byte().wrapping_add(1),
            self.address.get().high_byte()
        );
        let low_byte = self.read(pinout).await;
        self.address.set(high_address);
        let high_byte = self.read(pinout).await;
        self.address.set(u16::from_bytes(low_byte, high_byte));
    }

    /**
     * Indexed indirect reads the 16-bit target address from the memory found
     * at a zero page X-indexed address.
     */
    async fn indirect_x(&self, pinout: &impl Pinout) {
        self.zeropage_x(pinout).await;
        let low_byte = self.read(pinout).await;
        self.address.set(self.address.get().low_byte().wrapping_add(1) as u16);
        let high_byte = self.read(pinout).await;
        self.address.set(u16::from_bytes(low_byte, high_byte));
    }

    /**
     * Indirect indexed adds the Y register to the 16-bit address read from the
     * location found using zero page indexing.
     * If a page boundary is crossed when adding Y, an extra cycle is used.
     */
    async fn indirect_y_read(&self, pinout: &impl Pinout) {
        self.zeropage(pinout).await;
        let low_byte = self.read(pinout).await;
        self.address.set(self.address.get().low_byte().wrapping_add(1) as u16);
        let high_byte = self.read(pinout).await;
        let address = u16::from_bytes(low_byte, high_byte);
        self.address.set(address.wrapping_add(self.y.get() as u16));
        self.adjust_page(address);
    }

    /**
     * Indirect indexed adds the Y register to the 16-bit address read from the
     * location found using zero page indexing.
     * Write instructions addressed using indirect Y always use an additional
     * cycle to correct for a page crossing when adding Y.
     */
    async fn indirect_y_write(&self, pinout: &impl Pinout) {
        self.zeropage(pinout).await;
        let low_byte = self.read(pinout).await;
        self.address.set(self.address.get().low_byte().wrapping_add(1) as u16);
        let high_byte = self.read(pinout).await;
        self.address.set(u16::from_bytes(low_byte, high_byte).wrapping_add(self.y.get() as u16));
        self.time.tick();
    }


    /**
     * Logical AND of the accumulator with a byte of memory. The result is
     * stored in the accumulator.
     */
    fn and(&self) {
        self.a.set(self.a.get() & self.operand.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Add with carry.
     */
    fn adc(&self) {
        let a = self.a.get();
        let operand = self.operand.get();
        let result = a as u16 + operand as u16 + self.status.carry.get() as u16;
        self.status.carry.set(result > 0xff);
        self.status.zero.set(result.low_byte() == 0);
        self.status.overflow.set((operand.bit(7) == a.bit(7)) && (operand.bit(7) != result.bit(7)));
        self.status.negative.set(result.bit(7));
        self.a.set(result.low_byte());
    }

    /**
     * Arithmetic shift left.
     */
    fn asl(&self) {
        self.status.carry.set(self.operand.get().bit(7));
        self.operand.set(self.operand.get() << 1);
        self.status.zero.set(self.operand.get() == 0);
        self.status.negative.set(self.operand.get().bit(7));
        self.time.tick();
    }

    /**
     * Bit test
     */
    fn bit(&self) {
        self.status.zero.set(self.a.get() & self.operand.get() == 0);
        self.status.overflow.set(self.operand.get().bit(6));
        self.status.negative.set(self.operand.get().bit(7));
    }

    /**
     * Compare accumulator with memory
     */
    fn cmp(&self) {
        let result = self.a.get().wrapping_sub(self.operand.get());
        self.status.zero.set(result == 0);
        self.status.carry.set(self.a.get() >= self.operand.get());
        self.status.negative.set(result.bit(7));
    }

    /**
     * Compare X register with memory
     */
    fn cpx(&self) {
        let result = self.x.get().wrapping_sub(self.operand.get());
        self.status.zero.set(result == 0);
        self.status.carry.set(self.x.get() >= self.operand.get());
        self.status.negative.set(result.bit(7));
    }

    /**
     * Compare Y register with memory
     */
    fn cpy(&self) {
        let result = self.y.get().wrapping_sub(self.operand.get());
        self.status.zero.set(result == 0);
        self.status.carry.set(self.y.get() >= self.operand.get());
        self.status.negative.set(result.bit(7));
    }

    /**
     * Decrement memory
     */
    fn dec(&self) {
        self.operand.set(self.operand.get().wrapping_sub(1));
        self.status.zero.set(self.operand.get() == 0);
        self.status.negative.set(self.operand.get().bit(7));
        self.time.tick();
    }

    /**
     * Decrement X register
     */
    fn dex(&self) {
        self.time.tick();
        self.x.set(self.x.get().wrapping_sub(1));
        self.status.zero.set(self.x.get() == 0);
        self.status.negative.set(self.x.get().bit(7));
    }

    /**
     * Decrement Y register
     */
    fn dey(&self) {
        self.time.tick();
        self.y.set(self.y.get().wrapping_sub(1));
        self.status.zero.set(self.y.get() == 0);
        self.status.negative.set(self.y.get().bit(7));
    }

    /**
     * Exclusive OR
     */
    fn eor(&self) {
        self.a.set(self.a.get() ^ self.operand.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Increment memory
     */
    fn inc(&self) {
        self.operand.set(self.operand.get().wrapping_add(1));
        self.status.zero.set(self.operand.get() == 0);
        self.status.negative.set(self.operand.get().bit(7));
        self.time.tick();
    }

    /**
     * Increment X register
     */
    fn inx(&self) {
        self.time.tick();
        self.x.set(self.x.get().wrapping_add(1));
        self.status.zero.set(self.x.get() == 0);
        self.status.negative.set(self.x.get().bit(7));
    }

    /**
     * Increment Y register
     */
    fn iny(&self) {
        self.time.tick();
        self.y.set(self.y.get().wrapping_add(1));
        self.status.zero.set(self.y.get() == 0);
        self.status.negative.set(self.y.get().bit(7));
    }

    /**
     * Logical shift right
     */
    fn lsr(&self) {
        self.status.carry.set(self.operand.get().bit(0));
        self.operand.set(self.operand.get() >> 1);
        self.status.zero.set(self.operand.get() == 0);
        self.status.negative.set(self.operand.get().bit(7));
        self.time.tick();
    }

    /**
     * No operation
     */
    fn nop(&self) {
        self.time.tick();
    }

    /**
     * Logical inclusive OR
     */
    fn ora(&self) {
        self.a.set(self.a.get() | self.operand.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Rotate left
     */
    fn rol(&self) {
        let result = (self.operand.get() << 1).change_bit(0, self.status.carry.get());
        self.status.carry.set(self.operand.get().bit(7));
        self.status.zero.set(result == 0);
        self.status.negative.set(result.bit(7));
        self.operand.set(result);
        self.time.tick();
    }

    /**
     * Rotate right
     */
    fn ror(&self) {
        let result = (self.operand.get() >> 1).change_bit(7, self.status.carry.get());
        self.status.carry.set(self.operand.get().bit(0));
        self.status.zero.set(result == 0);
        self.status.negative.set(result.bit(7));
        self.operand.set(result);
        self.time.tick();
    }

    /**
     * Subtract with carry
     * Turns out that SBC(x) = ADC(!x), because of the nature of bitwise
     * addition and subtraction for two's complement numbers.
     */
    fn sbc(&self) {
        let result = self.a.get() as u16 + !self.operand.get() as u16 + self.status.carry.get() as u16;
        self.status.carry.set(result > 0xff);
        self.status.zero.set(result.low_byte() == 0);
        self.status.overflow.set((!self.operand.get().bit(7) == self.a.get().bit(7)) && (!self.operand.get().bit(7) != result.bit(7)));
        self.status.negative.set(result.bit(7));
        self.a.set(result.low_byte());
    }

    /**
     * LAX, similar to LDA followed by TAX
     */
    fn lax(&self) {
        self.a.set(self.operand.get());
        self.x.set(self.operand.get());
        self.status.zero.set(self.operand.get() == 0);
        self.status.negative.set(self.operand.get().bit(7));
    }

    /**
     * Loads memory into the accumulator register
     */
    fn lda(&self) {
        self.a.set(self.operand.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Loads memory into the X register
     */
    fn ldx(&self) {
        self.x.set(self.operand.get());
        self.status.zero.set(self.x.get() == 0);
        self.status.negative.set(self.x.get().bit(7));
    }

    /**
     * Loads memory into the Y register
     */
    fn ldy(&self) {
        self.y.set(self.operand.get());
        self.status.zero.set(self.y.get() == 0);
        self.status.negative.set(self.y.get().bit(7));
    }

    /**
     * BRK - Force interrupt
     */
    async fn brk(&self, pinout: &impl Pinout) {
        self.time.tick(); // Dummy read
        self.push(pinout, self.program_counter.get().high_byte()).await;
        self.push(pinout, self.program_counter.get().low_byte()).await;
        self.push(pinout, self.status.instruction_value()).await;
        self.address.set(0xfffe);
        self.program_counter.set(Word::from_bytes(
            self.fetch(pinout).await,
            self.read(pinout).await,
        ));
    }

    /**
     * JMP - Jump
     */
    fn jmp(&self) {
        self.program_counter.set(self.address.get());
    }
    
    /**
     * JSR - Jump to subroutine
     */
    async fn jsr(&self, pinout: &impl Pinout) {
        let low_byte = self.fetch(pinout).await;
        self.time.tick();
        self.push(pinout, self.program_counter.get().high_byte()).await;
        self.push(pinout, self.program_counter.get().low_byte()).await;
        let high_byte = self.fetch(pinout).await;
        self.program_counter.set(Word::from_bytes(low_byte, high_byte));
    }

    /**
     * PHA - Push accumulator
     */
    async fn pha(&self, pinout: &impl Pinout) {
        self.time.tick();
        self.push(pinout, self.a.get()).await;
    }

    /**
     * PHP - Push processor status
     */
    async fn php(&self, pinout: &impl Pinout) {
        self.time.tick();
        self.push(pinout, self.status.instruction_value()).await;
    }

    /**
     * PLA - Pull accumulator
     */
    async fn pla(&self, pinout: &impl Pinout) {
        self.time.tick();
        self.time.tick();
        self.a.set(self.pull(pinout).await);
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * PLP - Pull processor status
     */
    async fn plp(&self, pinout: &impl Pinout) {
        self.time.tick();
        self.time.tick();
        self.status.set(self.pull(pinout).await.into());
    }

    /**
     * RTI - Return from interrupt
     */
    async fn rti(&self, pinout: &impl Pinout) {
        self.time.tick(); // Dummy read
        self.time.tick(); // Pre-increment stack pointer
        self.status.set(self.pull(pinout).await.into());
        self.program_counter.set(u16::from_bytes(
            self.pull(pinout).await,
            self.pull(pinout).await
        ));
    }

    /**
     * RTS - Return from subroutine
     */
    async fn rts(&self, pinout: &impl Pinout) {
        self.time.tick(); // Dummy read
        self.time.tick(); // Pre-increment stack pointer
        self.program_counter.set(u16::from_bytes(
            self.pull(pinout).await,
            self.pull(pinout).await
        ).wrapping_add(1));
        self.time.tick();
    }

    /**
     * Transfer accumulator to X register
     */
    fn tax(&self) {
        self.time.tick(); // Dummy read
        self.x.set(self.a.get());
        self.status.zero.set(self.x.get() == 0);
        self.status.negative.set(self.x.get().bit(7));
    }

    /**
     * Transfer accumulator to Y register
     */
    fn tay(&self) {
        self.time.tick(); // Dummy read
        self.y.set(self.a.get());
        self.status.zero.set(self.y.get() == 0);
        self.status.negative.set(self.y.get().bit(7));
    }

    /**
     * Transfer stack pointer to X register
     */
    fn tsx(&self) {
        self.time.tick(); // Dummy read
        self.x.set(self.stack_pointer.get());
        self.status.zero.set(self.x.get() == 0);
        self.status.negative.set(self.x.get().bit(7));
    }

    /**
     * Transfer X register to accumulator
     */
    fn txa(&self) {
        self.time.tick(); // Dummy read
        self.a.set(self.x.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Transfer X register to stack pointer
     */
    fn txs(&self) {
        self.time.tick(); // Dummy read
        self.stack_pointer.set(self.x.get());
    }

    /**
     * Transfer Y register to accumulator
     */
    fn tya(&self) {
        self.time.tick(); // Dummy read
        self.a.set(self.y.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Stores the accumulator in memory
     */
    async fn sta(&self, pinout: &impl Pinout) {
        self.write(pinout, self.a.get()).await;
    }

    /**
     * Stores the X register in memory
     */
    async fn stx(&self, pinout: &impl Pinout) {
        self.write(pinout, self.x.get()).await;
    }

    /**
     * Stores the Y register in memory
     */
    async fn sty(&self, pinout: &impl Pinout) {
        self.write(pinout, self.y.get()).await;
    }

    /**
     * Stores the bitwise AND of the X and accumulator register in memory
     */
    async fn sax(&self, pinout: &impl Pinout) {
        self.write(pinout, self.a.get() & self.x.get()).await;
    }

    /**
     * Immediate AND, followed by N being copied to C.
     * TODO: Implement tests to ensure correctness.
     */
    fn anc(&self) {
        self.and();
        self.status.carry.set(self.status.negative.get());
    }

    /**
     * Immediate AND followed by LSR A.
     * Cannot be implemented in terms of those two instructions because of
     * timing problems, however.
     * TODO: Implement tests to ensure correctness.
     */
    fn alr(&self) {
        self.a.set(self.a.get() & self.operand.get());
        self.status.carry.set(self.a.get().bit(0));
        self.a.set(self.a.get() >> 1);
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }

    /**
     * Similar to immediate AND followed by ROR A, except for different flags:
     * - N, Z are normal
     * - C is bit 6 of the accumulator
     * - V is bit 6 xor bit 5 of the accumulator
     * TODO: Implement tests to ensure correctness.
     */
    fn arr(&self) {
        self.a.set(self.a.get() & self.operand.get());
        self.a.set((self.a.get() >> 1).change_bit(7, self.status.carry.get()));
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
        self.status.carry.set(self.a.get().bit(6));
        self.status.overflow.set(self.a.get().bit(6) ^ self.a.get().bit(5));
    }

    /**
     * Sets X to A&X minus immediate value, updating the appropriate arithmetic
     * flags.
     * TODO: Implement tests to ensure correctness.
     */
    fn axs(&self) {
        let (result, carry) = (self.a.get() & self.x.get()).overflowing_sub(self.operand.get());
        self.x.set(result);
        self.status.negative.set(self.x.get().bit(7));
        self.status.zero.set(self.x.get() == 0);
        self.status.carry.set(carry);
    }

    /**
     * TXA followed by immediate AND, but with one less cycle.
     * TODO: Implement tests to ensure correctness.
     */
    fn xaa(&self) {
        self.a.set(self.x.get());
        self.a.set(self.a.get() & self.operand.get());
        self.status.zero.set(self.a.get() == 0);
        self.status.negative.set(self.a.get().bit(7));
    }
}

impl std::fmt::Debug for Ricoh2A03 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "\n${:04x}\tA:${:02x} X:${:02x} Y:${:02x} P:{:?} SP:${:02x} CYC:{}",
            self.program_counter.get(),
            self.a.get(),
            self.x.get(),
            self.y.get(),
            self.status,
            self.stack_pointer.get(),
            self.time.current()
        )?;
        Ok(())
    }
}

/**
 * We compare the CPU's state by looking at only part of its registers,
 * since the address and operand are internal registers which can differ
 * between implementations (if they even exist).
 */
#[cfg(test)]
impl PartialEq for Ricoh2A03 {
    fn eq(&self, rhs: &Self) -> bool {
        self.time == rhs.time
        && self.status == rhs.status
        && self.program_counter == rhs.program_counter
        && self.stack_pointer == rhs.stack_pointer
        && self.a == rhs.a
        && self.x == rhs.x
        && self.y == rhs.y
    }
}

#[cfg(test)]
impl Eq for Ricoh2A03 {}

/**
 * Clone is needed to construct a history of CPU states.
 */
#[cfg(test)]
impl Clone for Ricoh2A03 {
    fn clone(&self) -> Self {
        Self {
            time: self.time.clone(),
            status: self.status.clone(),
            program_counter: self.program_counter.clone(),
            stack_pointer: self.stack_pointer.clone(),
            a: self.a.clone(),
            x: self.x.clone(),
            y: self.y.clone(),
            operand: self.operand.clone(),
            address: self.address.clone(),
        }
    }
}


/**
 * Processor status is kept track of in a special 8-bit register. While the
 * flags are mostly set and checked separately, they are pushed on and pulled
 * from the stack as a single byte.
 * 
 * Since the status is pushed on and pulled from the stack only rarely, it is
 * faster to use bools for the flags, as this means that no bit twiddling is
 * required to set/clear them. Only when pushed/pulled, the bools are combined
 * the single byte that they really are.
 */
#[cfg_attr(test, derive(Clone, PartialEq, Eq))]
struct Status {
    carry: Cell<bool>,
    zero: Cell<bool>,
    interrupt_disable: Cell<bool>,
    decimal_mode: Cell<bool>,
    // Phantom bit: break flag
    // Phantom bit: unused, always 1
    overflow: Cell<bool>,
    negative: Cell<bool>,
}

impl Status {
    fn new() -> Self {
        Self {
            carry: false.into(),
            zero: false.into(),
            interrupt_disable: true.into(),
            decimal_mode: false.into(),
            overflow: false.into(),
            negative: false.into(),
        }
    }

    fn set(&self, byte: u8) {
        self.carry.set(byte.bit(0));
        self.zero.set(byte.bit(1));
        self.interrupt_disable.set(byte.bit(2));
        self.decimal_mode.set(byte.bit(3));
        self.overflow.set(byte.bit(6));
        self.negative.set(byte.bit(7));
    }

    /**
     * When the status register is pushed on the stack, its value differs
     * depending on whether the status is pushed because of an instruction (PHP
     * or BRK) or an interrupt (IRQ or NMI).
     * For an instruction, the break flag, bit 4, is set.
     */
    fn instruction_value(&self) -> u8 {
        self.interrupt_value() | 0x10
    }

    /**
     * When the status register is pushed on the stack, its value differs
     * depending on whether the status is pushed because of an instruction (PHP
     * or BRK) or an interrupt (IRQ or NMI).
     * For an interrupt, the break flag, bit 4, is cleared.
     */
    fn interrupt_value(&self) -> u8 {
        self.carry.get() as u8 |
        (self.zero.get() as u8) << 1 |
        (self.interrupt_disable.get() as u8) << 2 |
        (self.decimal_mode.get() as u8) << 3 |
        (self.overflow.get() as u8) << 6 |
        (self.negative.get() as u8) << 7 |
        0x20
    }
}

impl std::convert::From<u8> for Status {
    fn from(byte: u8) -> Self {
        Self {
            carry: byte.bit(0).into(),
            zero:  byte.bit(1).into(),
            interrupt_disable: byte.bit(2).into(),
            decimal_mode: byte.bit(3).into(),
            overflow: byte.bit(6).into(),
            negative: byte.bit(7).into(),
        }
    }
}

impl std::fmt::Debug for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        f.write_char(if self.negative.get() { 'N' } else { 'n' })?;
        f.write_char(if self.overflow.get() { 'V' } else { 'v' })?;
        f.write_str("--")?;
        f.write_char(if self.decimal_mode.get() { 'D' } else { 'd' })?;
        f.write_char(if self.interrupt_disable.get() { 'I' } else { 'i' })?;
        f.write_char(if self.zero.get() { 'Z' } else { 'z' })?;
        f.write_char(if self.carry.get() { 'C' } else { 'c' })?;
        write!(f, " (${:02x})", self.interrupt_value())?;
        Ok(())
    }
}

#[cfg(test)]
mod status {
    use super::*;

    #[test]
    fn initialization() {
        let status = Status::new();
        assert_eq!(status.interrupt_value(), 0b00100100);
        assert_eq!(status.instruction_value(), 0b00110100);
        assert_eq!(status.carry.get(), false);
        assert_eq!(status.zero.get(), false);
        assert_eq!(status.interrupt_disable.get(), true);
        assert_eq!(status.decimal_mode.get(), false);
        assert_eq!(status.overflow.get(), false);
        assert_eq!(status.negative.get(), false);
        assert_eq!(format!("{:?}", status), "nv--dIzc ($24)");
    }

    #[test]
    fn assignment() {
        let status = Status::new();
        status.carry.set(true);
        status.zero.set(true);
        status.interrupt_disable.set(true);
        status.decimal_mode.set(true);
        status.overflow.set(true);
        status.negative.set(true);
        assert_eq!(status.interrupt_value(), 0b11101111);
        assert_eq!(status.instruction_value(), 0b11111111);
        assert_eq!(format!("{:?}", status), "NV--DIZC ($ef)");

        status.carry.set(false);
        status.zero.set(false);
        status.interrupt_disable.set(false);
        status.decimal_mode.set(false);
        status.overflow.set(false);
        status.negative.set(false);
        assert_eq!(format!("{:?}", status), "nv--dizc ($20)");
    }

    #[test]
    fn conversion() {
        let status: Status = 0b11111111.into();
        assert_eq!(status.interrupt_value(), 0b11101111);
        assert_eq!(status.instruction_value(), 0b11111111);
        assert_eq!(status.carry.get(), true);
        assert_eq!(status.zero.get(), true);
        assert_eq!(status.interrupt_disable.get(), true);
        assert_eq!(status.decimal_mode.get(), true);
        assert_eq!(status.overflow.get(), true);
        assert_eq!(status.negative.get(), true);
        assert_eq!(format!("{:?}", status), "NV--DIZC ($ef)");

        let status: Status = 0b00000000.into();
        assert_eq!(status.interrupt_value(), 0b00100000);
        assert_eq!(status.instruction_value(), 0b00110000);
        assert_eq!(status.carry.get(), false);
        assert_eq!(status.zero.get(), false);
        assert_eq!(status.interrupt_disable.get(), false);
        assert_eq!(status.decimal_mode.get(), false);
        assert_eq!(status.overflow.get(), false);
        assert_eq!(status.negative.get(), false);
        assert_eq!(format!("{:?}", status), "nv--dizc ($20)");
    }        
}

#[cfg(test)]
mod instruction_set {
    use super::*;
    use std::cell::Cell;

    /**
     * For testing purposes, we here use a dummy bus that interfaces to only
     * 256 bytes of memory.
     */
    struct DummyBus {
        data: [Cell<u8>; 0x100],
    }

    impl DummyBus {
        fn new() -> Self {
            Self { data: unsafe { std::mem::transmute([0u8; 0x100]) } }
        }
    }

    impl Pinout for DummyBus {
        fn read(&self, address: u16, _time: &Clock) -> Option<u8> {
            Some(self.data[address as usize % 0x100].get())
        }

        fn write(&self, address: u16, data: u8, _time: &Clock) -> Option<()> {
            self.data[address as usize % 0x100].set(data);
            Some(())
        }

        fn nmi(&self, _time: &Clock) -> Option<bool> { Some(false) }
        fn irq(&self, _time: &Clock) -> Option<bool> { Some(false) }
        fn reset(&self, _time: &Clock) -> Option<bool> { Some(false) }
    }

    #[test]
    fn cycles() {
        let bus = DummyBus::new();
        let cpu = Ricoh2A03::new();

        macro_rules! check_cycles {
            ($opcode:expr, $cycles:expr, $instruction:expr, $addressing:expr) => {{
                cpu.program_counter.set(0xf0);
                cpu.x.set(0);
                cpu.y.set(0);

                let start = cpu.time.current();
                futures::executor::block_on(cpu.execute(&bus, $opcode));

                // Add one cycle to compensate for missing opcode read
                let cycles = cpu.time.current() + 1 - start;
                assert_eq!(
                    cycles, $cycles,
                    "Incorrect number of cycles for {} {}: \
                    is {}, should be {}, \n{:?}",
                    $instruction, $addressing, cycles, $cycles, cpu
                );
            }};

            // Branch instructions take a variable number of cycles, so must be
            // checked for each of those scenarios.
            // We check branch timing for three scenario's:
            // - Branch not taken: nominal amount of cycles
            // - Branch taken: nominal cycles plus one
            // - Branch taken to different page: nominal cycles plus two
            ($opcode:expr, $cycles:expr, $instruction:expr, $addressing:expr, $flag:ident, $value:expr) => {{
                cpu.status.$flag.set(!($value));
                check_cycles!($opcode, $cycles, $instruction, $addressing);

                cpu.status.$flag.set($value);
                bus.data[0xf0].set(0x00);
                check_cycles!($opcode, $cycles + 1, $instruction, $addressing);

                bus.data[0xf0].set(0x0f);
                check_cycles!($opcode, $cycles + 2, $instruction, $addressing);
            }};
        }

        // Timings taken from: obelisk.me.uk/6502/reference.html
        check_cycles!(0x00, 7, "BRK", "Implied");
        check_cycles!(0x01, 6, "ORA", "(Indirect,X)");
        check_cycles!(0x05, 3, "ORA", "Zeropage");
        check_cycles!(0x06, 5, "ASL", "Zeropage");
        check_cycles!(0x08, 3, "PHP", "Implied");
        check_cycles!(0x09, 2, "ORA", "Immediate");
        check_cycles!(0x0a, 2, "ASL", "Accumulator");
        check_cycles!(0x0b, 2, "ANC", "Immediate");
        check_cycles!(0x0d, 4, "ORA", "Absolute");
        check_cycles!(0x0e, 6, "ASL", "Absolute");
        check_cycles!(0x10, 2, "BPL", "Relative", negative, false);
        check_cycles!(0x11, 5, "ORA", "(Indirect),Y");
        check_cycles!(0x15, 4, "ORA", "Zeropage,X");
        check_cycles!(0x16, 6, "ASL", "Zeropage,X");
        check_cycles!(0x18, 2, "CLC", "Implied");
        check_cycles!(0x19, 4, "ORA", "Absolute,Y");
        check_cycles!(0x1d, 4, "ORA", "Absolute,X");
        check_cycles!(0x1e, 7, "ASL", "Absolute,X");
        check_cycles!(0x20, 6, "JSR", "Absolute");
        check_cycles!(0x21, 6, "AND", "(Indirect,X)");
        check_cycles!(0x24, 3, "BIT", "Zeropage");
        check_cycles!(0x25, 3, "AND", "Zeropage");
        check_cycles!(0x26, 5, "ROL", "Zeropage");
        check_cycles!(0x28, 4, "PLP", "Implied");
        check_cycles!(0x29, 2, "AND", "Immediate");
        check_cycles!(0x2a, 2, "ROL", "Accumulator");
        check_cycles!(0x2b, 2, "ANC", "Immediate");
        check_cycles!(0x2c, 4, "BIT", "Absolute");
        check_cycles!(0x2d, 4, "AND", "Absolute");
        check_cycles!(0x2e, 6, "ROL", "Absolute");
        check_cycles!(0x30, 2, "BMI", "Relative", negative, true);
        check_cycles!(0x31, 5, "AND", "(Indirect),Y");
        check_cycles!(0x35, 4, "AND", "Zeropage,X");
        check_cycles!(0x36, 6, "ROL", "Zeropage,X");
        check_cycles!(0x38, 2, "SEC", "Implied");
        check_cycles!(0x39, 4, "AND", "Absolute,Y");
        check_cycles!(0x3d, 4, "AND", "Absolute,X");
        check_cycles!(0x3e, 7, "ROL", "Absolute,X");
        check_cycles!(0x40, 6, "RTI", "Implied");
        check_cycles!(0x41, 6, "EOR", "(Indirect,X)");
        check_cycles!(0x45, 3, "EOR", "Zeropage");
        check_cycles!(0x46, 5, "LSR", "Zeropage");
        check_cycles!(0x48, 3, "PHA", "Implied");
        check_cycles!(0x49, 2, "EOR", "Immediate");
        check_cycles!(0x4a, 2, "LSR", "Accumulator");
        check_cycles!(0x4b, 2, "ALR", "Immediate");
        check_cycles!(0x4c, 3, "JMP", "Absolute");
        check_cycles!(0x4d, 4, "EOR", "Absolute");
        check_cycles!(0x4e, 6, "LSR", "Absolute");
        check_cycles!(0x50, 2, "BVC", "Relative", overflow, false);
        check_cycles!(0x51, 5, "EOR", "(Indirect),Y");
        check_cycles!(0x55, 4, "EOR", "Zeropage,X");
        check_cycles!(0x56, 6, "LSR", "Zeropage,X");
        check_cycles!(0x58, 2, "CLI", "Implied");
        check_cycles!(0x59, 4, "EOR", "Absolute,Y");
        check_cycles!(0x5d, 4, "EOR", "Absolute,X");
        check_cycles!(0x5e, 7, "LSR", "Absolute,X");
        check_cycles!(0x60, 6, "RTS", "Implied");
        check_cycles!(0x61, 6, "ADC", "(Indirect,X)");
        check_cycles!(0x65, 3, "ADC", "Zeropage");
        check_cycles!(0x66, 5, "ROR", "Zeropage");
        check_cycles!(0x68, 4, "PLA", "Implied");
        check_cycles!(0x69, 2, "ADC", "Immediate");
        check_cycles!(0x6a, 2, "ROR", "Accumulator");
        check_cycles!(0x6b, 2, "ARR", "Immediate");
        check_cycles!(0x6c, 5, "JMP", "Indirect");
        check_cycles!(0x6d, 4, "ADC", "Absolute");
        check_cycles!(0x6e, 6, "ROR", "Absolute");
        check_cycles!(0x70, 2, "BVS", "Relative", overflow, true);
        check_cycles!(0x71, 5, "ADC", "(Indirect),Y");
        check_cycles!(0x75, 4, "ADC", "Zeropage,X");
        check_cycles!(0x76, 6, "ROR", "Zeropage,X");
        check_cycles!(0x78, 2, "SEI", "Implied");
        check_cycles!(0x79, 4, "ADC", "Absolute,Y");
        check_cycles!(0x7d, 4, "ADC", "Absolute,X");
        check_cycles!(0x7e, 7, "ROR", "Absolute,X");
        check_cycles!(0x80, 2, "NOP", "Immediate");
        check_cycles!(0x81, 6, "STA", "(Indirect,X)");
        check_cycles!(0x82, 2, "NOP", "Immediate");
        check_cycles!(0x84, 3, "STY", "Zeropage");
        check_cycles!(0x85, 3, "STA", "Zeropage");
        check_cycles!(0x86, 3, "STX", "Zeropage");
        check_cycles!(0x88, 2, "DEY", "Implied");
        check_cycles!(0x8a, 2, "TXA", "Implied");
        check_cycles!(0x8b, 2, "XAA", "Immediate");
        check_cycles!(0x8c, 4, "STY", "Absolute");
        check_cycles!(0x8d, 4, "STA", "Absolute");
        check_cycles!(0x8e, 4, "STX", "Absolute");
        check_cycles!(0x90, 2, "BCC", "Relative", carry, false);
        check_cycles!(0x91, 6, "STA", "(Indirect),Y");
        check_cycles!(0x94, 4, "STY", "Zeropage,X");
        check_cycles!(0x95, 4, "STA", "Zeropage,X");
        check_cycles!(0x96, 4, "STX", "Zeropage,X");
        check_cycles!(0x98, 2, "TYA", "Implied");
        check_cycles!(0x99, 5, "STA", "Absolute,Y");
        check_cycles!(0x9a, 2, "TXS", "Implied");
        check_cycles!(0x9d, 5, "STA", "Absolute,X");
        check_cycles!(0xa0, 2, "LDY", "Immediate");
        check_cycles!(0xa1, 6, "LDA", "(Indirect,X)");
        check_cycles!(0xa2, 2, "LDX", "Immediate");
        check_cycles!(0xa4, 3, "LDY", "Zeropage");
        check_cycles!(0xa5, 3, "LDA", "Zeropage");
        check_cycles!(0xa6, 3, "LDX", "Zeropage");
        check_cycles!(0xa8, 2, "TAY", "Implied");
        check_cycles!(0xa9, 2, "LDA", "Immediate");
        check_cycles!(0xaa, 2, "TAX", "Implied");
        check_cycles!(0xac, 4, "LDY", "Absolute");
        check_cycles!(0xad, 4, "LDA", "Absolute");
        check_cycles!(0xae, 4, "LDX", "Absolute");
        check_cycles!(0xb0, 2, "BCS", "Relative", carry, true);
        check_cycles!(0xb1, 5, "LDA", "(Indirect),Y");
        check_cycles!(0xb4, 4, "LDY", "Zeropage,X");
        check_cycles!(0xb5, 4, "LDA", "Zeropage,X");
        check_cycles!(0xb6, 4, "LDX", "Zeropage,Y");
        check_cycles!(0xb8, 2, "CLV", "Implied");
        check_cycles!(0xb9, 4, "LDA", "Absolute,Y");
        check_cycles!(0xba, 2, "TSX", "Implied");
        check_cycles!(0xbc, 4, "LDY", "Absolute,X");
        check_cycles!(0xbd, 4, "LDA", "Absolute,X");
        check_cycles!(0xbe, 4, "LDX", "Absolute,Y");
        check_cycles!(0xc0, 2, "CPY", "Immediate");
        check_cycles!(0xc1, 6, "CMP", "(Indirect,X)");
        check_cycles!(0xc2, 2, "NOP", "Immediate");
        check_cycles!(0xc4, 3, "CPY", "Zeropage");
        check_cycles!(0xc5, 3, "CMP", "Zeropage");
        check_cycles!(0xc6, 5, "DEC", "Zeropage");
        check_cycles!(0xc8, 2, "INY", "Implied");
        check_cycles!(0xc9, 2, "CMP", "Immediate");
        check_cycles!(0xca, 2, "DEX", "Implied");
        check_cycles!(0xcb, 2, "AXS", "Immediate");
        check_cycles!(0xcc, 4, "CPY", "Absolute");
        check_cycles!(0xcd, 4, "CMP", "Absolute");
        check_cycles!(0xce, 6, "DEC", "Absolute");
        check_cycles!(0xd0, 2, "BNE", "Relative", zero, false);
        check_cycles!(0xd1, 5, "CMP", "(Indirect),Y");
        check_cycles!(0xd5, 4, "CMP", "Zeropage,X");
        check_cycles!(0xd6, 6, "DEC", "Zeropage,X");
        check_cycles!(0xd8, 2, "CLD", "Implied");
        check_cycles!(0xd9, 4, "CMP", "Absolute,Y");
        check_cycles!(0xdd, 4, "CMP", "Absolute,X");
        check_cycles!(0xde, 7, "DEC", "Absolute,X");
        check_cycles!(0xe0, 2, "CPX", "Immediate");
        check_cycles!(0xe1, 6, "SBC", "(Indirect,X)");
        check_cycles!(0xe2, 2, "NOP", "Immediate");
        check_cycles!(0xe4, 3, "CPX", "Zeropage");
        check_cycles!(0xe5, 3, "SBC", "Zeropage");
        check_cycles!(0xe6, 5, "INC", "Zeropage");
        check_cycles!(0xe8, 2, "INX", "Implied");
        check_cycles!(0xe9, 2, "SBC", "Immediate");
        check_cycles!(0xea, 2, "NOP", "Implied");
        check_cycles!(0xec, 4, "CPX", "Absolute");
        check_cycles!(0xed, 4, "SBC", "Absolute");
        check_cycles!(0xee, 6, "INC", "Absolute");
        check_cycles!(0xf0, 2, "BEQ", "Relative", zero, true);
        check_cycles!(0xf1, 5, "SBC", "(Indirect),Y");
        check_cycles!(0xf5, 4, "SBC", "Zeropage,X");
        check_cycles!(0xf6, 6, "INC", "Zeropage,X");
        check_cycles!(0xf8, 2, "SED", "Implied");
        check_cycles!(0xf9, 4, "SBC", "Absolute,Y");
        check_cycles!(0xfd, 4, "SBC", "Absolute,X");
        check_cycles!(0xfe, 7, "INC", "Absolute,X");
    }

    #[test]
    fn bytes() {
        let bus = DummyBus::new();
        let cpu = Ricoh2A03::new();

        macro_rules! check_bytes {
            ($opcode:expr, $bytes:expr, $instruction:expr, $addressing:expr) => {{
                cpu.program_counter.set(0xf0);
                cpu.x.set(0);
                cpu.y.set(0);

                let first_byte = cpu.program_counter.get();
                futures::executor::block_on(cpu.execute(&bus, $opcode));

                // Add one byte to compensate for missing opcode read
                let bytes = cpu.program_counter.get() + 1 - first_byte;
                assert_eq!(
                    bytes, $bytes,
                    "Incorrect number of bytes for {} {}: \
                    is {}, should be {}, \n{:?}",
                    $instruction, $addressing, bytes, $bytes, cpu
                );
            }};


            // Branch instructions can be checked for number of bytes used only
            // when the branch is not taken, as the program counter is shifted
            // otherwise.
            ($opcode:expr, $bytes:expr, $instruction:expr, $addressing:expr, $flag:ident, $value:expr) => {{
                cpu.status.$flag.set(!$value);
                check_bytes!($opcode, $bytes, $instruction, $addressing);
            }}
        }

        // Bytes taken from: obelisk.me.uk/6502/reference.html
        check_bytes!(0x01, 2, "ORA", "(Indirect,X)");
        check_bytes!(0x05, 2, "ORA", "Zeropage");
        check_bytes!(0x06, 2, "ASL", "Zeropage");
        check_bytes!(0x08, 1, "PHP", "Implied");
        check_bytes!(0x09, 2, "ORA", "Immediate");
        check_bytes!(0x0a, 1, "ASL", "Accumulator");
        check_bytes!(0x0b, 2, "ANC", "Immediate");
        check_bytes!(0x0d, 3, "ORA", "Absolute");
        check_bytes!(0x0e, 3, "ASL", "Absolute");
        check_bytes!(0x10, 2, "BPL", "Relative", negative, false);
        check_bytes!(0x11, 2, "ORA", "(Indirect),Y");
        check_bytes!(0x15, 2, "ORA", "Zeropage,X");
        check_bytes!(0x16, 2, "ASL", "Zeropage,X");
        check_bytes!(0x18, 1, "CLC", "Implied");
        check_bytes!(0x19, 3, "ORA", "Absolute,Y");
        check_bytes!(0x1d, 3, "ORA", "Absolute,X");
        check_bytes!(0x1e, 3, "ASL", "Absolute,X");
        check_bytes!(0x21, 2, "AND", "(Indirect,X)");
        check_bytes!(0x24, 2, "BIT", "Zeropage");
        check_bytes!(0x25, 2, "AND", "Zeropage");
        check_bytes!(0x26, 2, "ROL", "Zeropage");
        check_bytes!(0x28, 1, "PLP", "Implied");
        check_bytes!(0x29, 2, "AND", "Immediate");
        check_bytes!(0x2a, 1, "ROL", "Accumulator");
        check_bytes!(0x2b, 2, "ANC", "Immediate");
        check_bytes!(0x2c, 3, "BIT", "Absolute");
        check_bytes!(0x2d, 3, "AND", "Absolute");
        check_bytes!(0x2e, 3, "ROL", "Absolute");
        check_bytes!(0x30, 2, "BMI", "Relative", negative, true);
        check_bytes!(0x31, 2, "AND", "(Indirect),Y");
        check_bytes!(0x35, 2, "AND", "Zeropage,X");
        check_bytes!(0x36, 2, "ROL", "Zeropage,X");
        check_bytes!(0x38, 1, "SEC", "Implied");
        check_bytes!(0x39, 3, "AND", "Absolute,Y");
        check_bytes!(0x3d, 3, "AND", "Absolute,X");
        check_bytes!(0x3e, 3, "ROL", "Absolute,X");
        check_bytes!(0x41, 2, "EOR", "(Indirect,X)");
        check_bytes!(0x45, 2, "EOR", "Zeropage");
        check_bytes!(0x46, 2, "LSR", "Zeropage");
        check_bytes!(0x48, 1, "PHA", "Implied");
        check_bytes!(0x49, 2, "EOR", "Immediate");
        check_bytes!(0x4a, 1, "LSR", "Accumulator");
        check_bytes!(0x4b, 2, "ALR", "Immediate");
        check_bytes!(0x4d, 3, "EOR", "Absolute");
        check_bytes!(0x4e, 3, "LSR", "Absolute");
        check_bytes!(0x50, 2, "BVC", "Relative", overflow, false);
        check_bytes!(0x51, 2, "EOR", "(Indirect),Y");
        check_bytes!(0x55, 2, "EOR", "Zeropage,X");
        check_bytes!(0x56, 2, "LSR", "Zeropage,X");
        check_bytes!(0x58, 1, "CLI", "Implied");
        check_bytes!(0x59, 3, "EOR", "Absolute,Y");
        check_bytes!(0x5d, 3, "EOR", "Absolute,X");
        check_bytes!(0x5e, 3, "LSR", "Absolute,X");
        check_bytes!(0x61, 2, "ADC", "(Indirect,X)");
        check_bytes!(0x65, 2, "ADC", "Zeropage");
        check_bytes!(0x66, 2, "ROR", "Zeropage");
        check_bytes!(0x68, 1, "PLA", "Implied");
        check_bytes!(0x69, 2, "ADC", "Immediate");
        check_bytes!(0x6a, 1, "ROR", "Accumulator");
        check_bytes!(0x6b, 2, "ARR", "Immediate");
        check_bytes!(0x6d, 3, "ADC", "Absolute");
        check_bytes!(0x6e, 3, "ROR", "Absolute");
        check_bytes!(0x70, 2, "BVS", "Relative", overflow, true);
        check_bytes!(0x71, 2, "ADC", "(Indirect),Y");
        check_bytes!(0x75, 2, "ADC", "Zeropage,X");
        check_bytes!(0x76, 2, "ROR", "Zeropage,X");
        check_bytes!(0x78, 1, "SEI", "Implied");
        check_bytes!(0x79, 3, "ADC", "Absolute,Y");
        check_bytes!(0x7d, 3, "ADC", "Absolute,X");
        check_bytes!(0x7e, 3, "ROR", "Absolute,X");
        check_bytes!(0x80, 2, "NOP", "Immediate");
        check_bytes!(0x81, 2, "STA", "(Indirect,X)");
        check_bytes!(0x82, 2, "NOP", "Immediate");
        check_bytes!(0x84, 2, "STY", "Zeropage");
        check_bytes!(0x85, 2, "STA", "Zeropage");
        check_bytes!(0x86, 2, "STX", "Zeropage");
        check_bytes!(0x88, 1, "DEY", "Implied");
        check_bytes!(0x8a, 1, "TXA", "Implied");
        check_bytes!(0x8b, 2, "XAA", "Immediate");
        check_bytes!(0x8c, 3, "STY", "Absolute");
        check_bytes!(0x8d, 3, "STA", "Absolute");
        check_bytes!(0x8e, 3, "STX", "Absolute");
        check_bytes!(0x90, 2, "BCC", "Relative", carry, false);
        check_bytes!(0x91, 2, "STA", "(Indirect),Y");
        check_bytes!(0x94, 2, "STY", "Zeropage,X");
        check_bytes!(0x95, 2, "STA", "Zeropage,X");
        check_bytes!(0x96, 2, "STX", "Zeropage,X");
        check_bytes!(0x98, 1, "TYA", "Implied");
        check_bytes!(0x99, 3, "STA", "Absolute,Y");
        check_bytes!(0x9a, 1, "TXS", "Implied");
        check_bytes!(0x9d, 3, "STA", "Absolute,X");
        check_bytes!(0xa0, 2, "LDY", "Immediate");
        check_bytes!(0xa1, 2, "LDA", "(Indirect,X)");
        check_bytes!(0xa2, 2, "LDX", "Immediate");
        check_bytes!(0xa4, 2, "LDY", "Zeropage");
        check_bytes!(0xa5, 2, "LDA", "Zeropage");
        check_bytes!(0xa6, 2, "LDX", "Zeropage");
        check_bytes!(0xa8, 1, "TAY", "Implied");
        check_bytes!(0xa9, 2, "LDA", "Immediate");
        check_bytes!(0xaa, 1, "TAX", "Implied");
        check_bytes!(0xac, 3, "LDY", "Absolute");
        check_bytes!(0xad, 3, "LDA", "Absolute");
        check_bytes!(0xae, 3, "LDX", "Absolute");
        check_bytes!(0xb0, 2, "BCS", "Relative", carry, true);
        check_bytes!(0xb1, 2, "LDA", "(Indirect),Y");
        check_bytes!(0xb4, 2, "LDY", "Zeropage,X");
        check_bytes!(0xb5, 2, "LDA", "Zeropage,X");
        check_bytes!(0xb6, 2, "LDX", "Zeropage,Y");
        check_bytes!(0xb8, 1, "CLV", "Implied");
        check_bytes!(0xb9, 3, "LDA", "Absolute,Y");
        check_bytes!(0xba, 1, "TSX", "Implied");
        check_bytes!(0xbc, 3, "LDY", "Absolute,X");
        check_bytes!(0xbd, 3, "LDA", "Absolute,X");
        check_bytes!(0xbe, 3, "LDX", "Absolute,Y");
        check_bytes!(0xc0, 2, "CPY", "Immediate");
        check_bytes!(0xc1, 2, "CMP", "(Indirect,X)");
        check_bytes!(0xc2, 2, "NOP", "Immediate");
        check_bytes!(0xc4, 2, "CPY", "Zeropage");
        check_bytes!(0xc5, 2, "CMP", "Zeropage");
        check_bytes!(0xc6, 2, "DEC", "Zeropage");
        check_bytes!(0xc8, 1, "INY", "Implied");
        check_bytes!(0xc9, 2, "CMP", "Immediate");
        check_bytes!(0xca, 1, "DEX", "Implied");
        check_bytes!(0xcb, 2, "AXS", "Immediate");
        check_bytes!(0xcc, 3, "CPY", "Absolute");
        check_bytes!(0xcd, 3, "CMP", "Absolute");
        check_bytes!(0xce, 3, "DEC", "Absolute");
        check_bytes!(0xd0, 2, "BNE", "Relative", zero, false);
        check_bytes!(0xd1, 2, "CMP", "(Indirect),Y");
        check_bytes!(0xd5, 2, "CMP", "Zeropage,X");
        check_bytes!(0xd6, 2, "DEC", "Zeropage,X");
        check_bytes!(0xd8, 1, "CLD", "Implied");
        check_bytes!(0xd9, 3, "CMP", "Absolute,Y");
        check_bytes!(0xdd, 3, "CMP", "Absolute,X");
        check_bytes!(0xde, 3, "DEC", "Absolute,X");
        check_bytes!(0xe0, 2, "CPX", "Immediate");
        check_bytes!(0xe1, 2, "SBC", "(Indirect,X)");
        check_bytes!(0xe2, 2, "NOP", "Immediate");
        check_bytes!(0xe4, 2, "CPX", "Zeropage");
        check_bytes!(0xe5, 2, "SBC", "Zeropage");
        check_bytes!(0xe6, 2, "INC", "Zeropage");
        check_bytes!(0xe8, 1, "INX", "Implied");
        check_bytes!(0xe9, 2, "SBC", "Immediate");
        check_bytes!(0xea, 1, "NOP", "Implied");
        check_bytes!(0xec, 3, "CPX", "Absolute");
        check_bytes!(0xed, 3, "SBC", "Absolute");
        check_bytes!(0xee, 3, "INC", "Absolute");
        check_bytes!(0xf0, 2, "BEQ", "Relative", zero, true);
        check_bytes!(0xf1, 2, "SBC", "(Indirect),Y");
        check_bytes!(0xf5, 2, "SBC", "Zeropage,X");
        check_bytes!(0xf6, 2, "INC", "Zeropage,X");
        check_bytes!(0xf8, 1, "SED", "Implied");
        check_bytes!(0xf9, 3, "SBC", "Absolute,Y");
        check_bytes!(0xfd, 3, "SBC", "Absolute,X");
        check_bytes!(0xfe, 3, "INC", "Absolute,X");
    }

    #[test]
    fn jump() {
        let bus = {
            let bus = DummyBus::new();
            bus.write(0xc000, 0xff, &Clock::new());
            bus
        };
        let cpu = Ricoh2A03::new();
        cpu.program_counter.set(0xc000);

        futures::executor::block_on(cpu.execute(&bus, 0x4c));
        assert_eq!(cpu.program_counter.get(), 0x00ff);
    }
}