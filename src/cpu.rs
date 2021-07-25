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

/**
 * Emulated internals of the Ricoh 2A03.
 */
pub struct Ricoh2A03<'nes, Memory: Pinout> {
    clock: Clock,
    status: Status,
    program_counter: u16,
    stack_pointer: u8,
    a: u8,
    x: u8,
    y: u8,
    operand: u8, // Data bus register, combination of SB/DB
    address: u16, // Address bus register, combination of ADL/ADH/ABL/ABH
    bus: Option<&'nes Memory>, // Reference to the memory bus
}

impl<'nes, Memory: Pinout> Ricoh2A03<'nes, Memory> {
    pub fn reset(bus: &'nes Memory) -> Self {
        Self {
            clock: Clock::from(7), // Start-up takes 7 cycles
            program_counter: 0xc000,
            status: Status::new(),
            stack_pointer: 0xfd,
            a: 0x00,
            x: 0x00,
            y: 0x00,
            operand: 0x00,
            address: 0x0000,
            bus: Some(bus),
        }
    }

    pub fn new(cycle: u64, status: u8, program_counter: u16, stack_pointer: u8,
                a: u8, x: u8, y: u8, bus: Option<&'nes Memory>) -> Self {
        Self {
            clock: Clock::from(cycle),
            status: status.into(),
            program_counter,
            stack_pointer,
            a,
            x,
            y,
            operand: 0x00,
            address: 0x0000,
            bus: bus,
        }
    }

    /**
     * For testing purposes, we also allow a CPU to be constructed from a
     * Nintendulator log entry.
     */
    #[cfg(test)]
    pub fn from_nintendulator(log: &str) -> Self {
        let mut state = log.split_whitespace();
        let program_counter: u16 = u16::from_str_radix(state.next().unwrap(), 16).unwrap();
        let cycle: u64 = u64::from_str_radix(state.next_back().unwrap().strip_prefix("CYC:").unwrap(), 10).unwrap();

        let mut skip = state.next().unwrap().strip_prefix("A:");
        while skip.is_none() {
            skip = state.next().unwrap().strip_prefix("A:");
        }

        let a = u8::from_str_radix(skip.unwrap(), 16).unwrap();
        let x = u8::from_str_radix(state.next().unwrap().strip_prefix("X:").unwrap(), 16).unwrap();
        let y = u8::from_str_radix(state.next().unwrap().strip_prefix("Y:").unwrap(), 16).unwrap();
        let status = u8::from_str_radix(state.next().unwrap().strip_prefix("P:").unwrap(), 16).unwrap();
        let stack_pointer = u8::from_str_radix(state.next().unwrap().strip_prefix("SP:").unwrap(), 16).unwrap();

        Self::new(cycle, status, program_counter, stack_pointer, a, x, y, None)
    }

    pub async fn run(&mut self) {
        loop {
            self.step().await;
        }
    }

    pub fn cycle(&self) -> u64 {
        self.clock.current()
    }

    pub async fn step(&mut self) {
        let opcode = self.fetch().await;
        self.execute(opcode).await;
    }

    /**
     * Executes the instruction associated with the given opcode.
     * Implemented as a combination of macros for the addressing modes and
     * operation types.
     */
    async fn execute(&mut self, opcode: u8) {
        /**
         * All branch instructions follow a similar pattern, so we use a macro
         * to generate them.
         */
        macro_rules! branch {
            ($flag:ident, $value:expr) => {{
                self.relative().await;
                if self.status.$flag == $value {
                    self.clock.advance(1);
                    if self.address.high_byte() != self.program_counter.high_byte() {
                        self.clock.advance(1);
                    }
                    self.program_counter = self.address;
                }
            }};
        }

        /**
         * Sets a given status flag.
         */
        macro_rules! set {
            ($flag:ident, $value:expr) => {{
                self.clock.advance(1);
                self.status.$flag = $value;
            }}
        }

        /**
         * Address-based instructions operate on an address only, but don't
         * read from the corresponding memory location.
         */
        macro_rules! address {
            ($instruction:ident, $addressing:ident) => {{
                self.$addressing().await;
                self.$instruction().await;
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
                self.$addressing().await;
                self.operand = self.read().await;
                self.$instruction().await;
            }};
        }

        /**
         * Modify instructions write the result of an operation back to memory
         * in some way. This can be the memory bus, or the accumulator,
         * depending on the addressing mode.
         */
        macro_rules! modify {
            ($instruction:ident, accumulator) => {{
                self.operand = self.a;
                self.$instruction().await;
                self.a = self.operand;
            }};

            ($instruction:ident, $addressing:ident) => {{
                self.$addressing().await;
                self.operand = self.read().await;
                self.$instruction().await;
                self.write(self.operand).await;
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
                self.$addressing().await;
                self.operand = self.read().await;
                self.$modify().await;
                self.$read().await;
                self.write(self.operand).await;
            }}
        }

        /**
         * Implied instructions don't have a corresponding addressing mode.
         */
        macro_rules! implied {
            ($instruction:ident) => {{
                self.$instruction().await;
            }};
        }

        match opcode {
            0x00 => implied!(brk),
            0x01 => read!(ora, indirect_x),
            0x02 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x03 => combine!(asl, ora, indirect_x),
            0x04 => address!(nop, zeropage),
            0x05 => read!(ora, zeropage),
            0x06 => modify!(asl, zeropage),
            0x07 => combine!(asl, ora, zeropage),
            0x08 => implied!(php),
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
            0x20 => implied!(jsr),
            0x21 => read!(and, indirect_x),
            0x22 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x23 => combine!(rol, and, indirect_x),
            0x24 => read!(bit, zeropage),
            0x25 => read!(and, zeropage),
            0x26 => modify!(rol, zeropage),
            0x27 => combine!(rol, and, zeropage),
            0x28 => implied!(plp),
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
            0x40 => implied!(rti),
            0x41 => read!(eor, indirect_x),
            0x42 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x43 => combine!(lsr, eor, indirect_x),
            0x44 => address!(nop, zeropage),
            0x45 => read!(eor, zeropage),
            0x46 => modify!(lsr, zeropage),
            0x47 => combine!(lsr, eor, zeropage),
            0x48 => implied!(pha),
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
            0x60 => implied!(rts),
            0x61 => read!(adc, indirect_x),
            0x62 => unimplemented!("Encountered KIL opcode ${:x}, CPU state: {:?}", opcode, self),
            0x63 => combine!(ror, adc, indirect_x),
            0x64 => address!(nop, zeropage),
            0x65 => read!(adc, zeropage),
            0x66 => modify!(ror, zeropage),
            0x67 => combine!(ror, adc, zeropage),
            0x68 => implied!(pla),
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
            0x8b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
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
            0xab => read!(xaa, immediate),
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
    async fn read(&self) -> u8 {
        loop {
            match self.bus.unwrap().read(self.address, &self.clock) {
                None => yields().await,
                Some(data) => {
                    self.clock.advance(1);
                    return data;
                },
            }
        }
    }

    /**
     * Writes to the bus, taking one cycle.
     */
    async fn write(&self, data: u8) {
        loop {
            match self.bus.unwrap().write(self.address, data, &self.clock) {
                None => yields().await,
                Some(()) => return self.clock.advance(1),
            }
        }
    }

    /**
     * Pushes a value onto the stack. Takes one cycle.
     */
    async fn push(&mut self, data: u8) {
        self.address = 0x0100 | self.stack_pointer as u16;
        self.write(data).await;
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    /**
     * Pulls a value from the stack. Takes two cycles, or one if the previous
     * operation was also a pull. As such, the second cycle must be added
     * manually.
     */
    async fn pull(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.address = 0x0100 | self.stack_pointer as u16;
        self.read().await
    }

    /**
     * Reads a byte from the location of the program counter, and increments
     * the program counter afterwards. Takes one cycle, because of the read.
     */
    async fn fetch(&mut self) -> u8 {
        self.address = self.program_counter;
        self.program_counter = self.program_counter.wrapping_add(1);
        self.read().await
    }


    /**
     * Immediate addressing operates straight on the operand.
     */
    async fn immediate(&mut self) {
        self.address = self.program_counter;
        self.program_counter = self.program_counter.wrapping_add(1);
    }

    /**
     * Zeropage addressing interprets the operand as the effective address.
     */
    async fn zeropage(&mut self) {
        self.address = self.fetch().await as u16;
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by
     * adding an index register to the immediate operand, and
     * interpreting that as an address.
     * Note: the high byte is always zero, page boundary crossings wrap
     * around instead.
     */
    async fn zeropage_x(&mut self) {
        self.address = self.fetch().await.wrapping_add(self.x) as u16;
        self.clock.advance(1);
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by
     * adding an index register to the immediate operand, and
     * interpreting that as an address.
     * Note: the high byte is always zero, page boundary crossings wrap
     * around instead.
     */
    async fn zeropage_y(&mut self) {
        self.address = self.fetch().await.wrapping_add(self.y) as u16;
        self.clock.advance(1);
    }

    /**
     * Relative addressing loads the program counter, offset by a given
     * amount.
     */
    async fn relative(&mut self) {
        let offset = self.fetch().await as i8;
        self.address = self.program_counter.wrapping_add(offset as u16);
    }

    /**
     * Absolute addressing loads the two bytes after the opcode into a
     * 16-bit word that it uses as effective address.
     */
    async fn absolute(&mut self) {
        let low_byte = self.fetch().await;
        let high_byte = self.fetch().await;
        self.address = u16::from_bytes(low_byte, high_byte);
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Read instructions using the absolute X addressing mode take an extra
     * cycle if the address increment crosses a page.
     */
    async fn absolute_x_read(&mut self) {
        self.absolute().await;
        let address = self.address.wrapping_add(self.x as u16);
        let page_crossing = address.high_byte() != self.address.high_byte();
        if page_crossing {
            self.clock.advance(1);
        }
        self.address = address;
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Read instructions using the absolute Y addressing mode take an extra
     * cycle if the address increment crosses a page.
     */
    async fn absolute_y_read(&mut self) {
        self.absolute().await;
        let address = self.address.wrapping_add(self.y as u16);
        let page_crossing = address.high_byte() != self.address.high_byte();
        if page_crossing {
            self.clock.advance(1);
        }
        self.address = address;
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Read instructions using the absolute X addressing mode take an extra
     * cycle if the address increment crosses  apge.
     */
    async fn absolute_x_write(&mut self) {
        self.absolute().await;
        self.address = self.address.wrapping_add(self.x as u16);
        self.clock.advance(1);
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     * Write instructions using the absolute Y addressing mode always take an
     * extra cycle.
     */
    async fn absolute_y_write(&mut self) {
        self.absolute().await;
        self.address = self.address.wrapping_add(self.y as u16);
        self.clock.advance(1);
    }

    /**
     * Indirect addressing uses a 16-bit operand as address from which it reads
     * the actual address to operate on.
     * Is bugged in the sense that indirect vectors on a page boundary do not
     * correctly cross that page boundary, e.g. $00ff reads from ($00ff, $0000)
     * instead of ($00ff, $0100).
     */
    async fn indirect(&mut self) {
        self.absolute().await;
        let high_address = u16::from_bytes(
            self.address.low_byte().wrapping_add(1),
            self.address.high_byte()
        );
        let low_byte = self.read().await;
        self.address = high_address;
        let high_byte = self.read().await;
        self.address = u16::from_bytes(low_byte, high_byte)
    }

    /**
     * Indexed indirect reads the 16-bit target address from the memory found
     * at a zero page X-indexed address.
     */
    async fn indirect_x(&mut self) {
        self.zeropage_x().await;
        let low_byte = self.read().await;
        self.address = self.address.low_byte().wrapping_add(1) as u16;
        let high_byte = self.read().await;
        self.address = u16::from_bytes(low_byte, high_byte)
    }

    /**
     * Indirect indexed adds the Y register to the 16-bit address read from the
     * location found using zero page indexing.
     * If a page boundary is crossed when adding Y, an extra cycle is used.
     */
    async fn indirect_y_read(&mut self) {
        self.zeropage().await;
        let low_byte = self.read().await;
        self.address = self.address.low_byte().wrapping_add(1) as u16;
        let high_byte = self.read().await;
        let address = u16::from_bytes(low_byte, high_byte);
        self.address = address.wrapping_add(self.y as u16);

        let page_crossing = self.address.high_byte() != address.high_byte();
        if page_crossing {
            self.clock.advance(1);
        }
    }

    /**
     * Indirect indexed adds the Y register to the 16-bit address read from the
     * location found using zero page indexing.
     * Write instructions addressed using indirect Y always use an additional
     * cycle to correct for a page crossing when adding Y.
     */
    async fn indirect_y_write(&mut self) {
        self.zeropage().await;
        let low_byte = self.read().await;
        self.address = self.address.low_byte().wrapping_add(1) as u16;
        let high_byte = self.read().await;
        self.address = u16::from_bytes(low_byte, high_byte).wrapping_add(self.y as u16);
        self.clock.advance(1);
    }


    /**
     * Logical AND of the accumulator with a byte of memory. The result is
     * stored in the accumulator.
     */
    async fn and(&mut self) {
        self.a &= self.operand;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Add with carry.
     */
    async fn adc(&mut self) {
        let result = self.a as u16 + self.operand as u16 + self.status.carry as u16;
        self.status.carry = result > 0xff;
        self.status.zero = result.low_byte() == 0;
        self.status.overflow = (self.operand.bit(7) == self.a.bit(7)) && (self.operand.bit(7) != result.bit(7));
        self.status.negative = result.bit(7);
        self.a = result.low_byte();
    }

    /**
     * Arithmetic shift left.
     */
    async fn asl(&mut self) {
        self.status.carry = self.operand.bit(7);
        self.operand <<= 1;
        self.status.zero = self.operand == 0;
        self.status.negative = self.operand.bit(7);
        self.clock.advance(1);
    }

    /**
     * Bit test
     */
    async fn bit(&mut self) {
        self.status.zero = self.a & self.operand == 0;
        self.status.overflow = self.operand.bit(6);
        self.status.negative = self.operand.bit(7);
    }

    /**
     * Compare accumulator with memory
     */
    async fn cmp(&mut self) {
        let result = self.a.wrapping_sub(self.operand);
        self.status.zero = result == 0;
        self.status.carry = self.a >= self.operand;
        self.status.negative = result.bit(7);
    }

    /**
     * Compare X register with memory
     */
    async fn cpx(&mut self) {
        let result = self.x.wrapping_sub(self.operand);
        self.status.zero = result == 0;
        self.status.carry = self.x >= self.operand;
        self.status.negative = result.bit(7);
    }

    /**
     * Compare Y register with memory
     */
    async fn cpy(&mut self) {
        let result = self.y.wrapping_sub(self.operand);
        self.status.zero = result == 0;
        self.status.carry = self.y >= self.operand;
        self.status.negative = result.bit(7);
    }

    /**
     * Decrement memory
     */
    async fn dec(&mut self) {
        self.operand = self.operand.wrapping_sub(1);
        self.status.zero = self.operand == 0;
        self.status.negative = self.operand.bit(7);
        self.clock.advance(1);
    }

    /**
     * Decrement X register
     */
    async fn dex(&mut self) {
        self.clock.advance(1);
        self.x = self.x.wrapping_sub(1);
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Decrement Y register
     */
    async fn dey(&mut self) {
        self.clock.advance(1);
        self.y = self.y.wrapping_sub(1);
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * Exclusive OR
     */
    async fn eor(&mut self) {
        self.a ^= self.operand;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Increment memory
     */
    async fn inc(&mut self) {
        self.operand = self.operand.wrapping_add(1);
        self.status.zero = self.operand == 0;
        self.status.negative = self.operand.bit(7);
        self.clock.advance(1);
    }

    /**
     * Increment X register
     */
    async fn inx(&mut self) {
        self.clock.advance(1);
        self.x = self.x.wrapping_add(1);
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Increment Y register
     */
    async fn iny(&mut self) {
        self.clock.advance(1);
        self.y = self.y.wrapping_add(1);
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * Logical shift right
     */
    async fn lsr(&mut self) {
        self.status.carry = self.operand.bit(0);
        self.operand >>= 1;
        self.status.zero = self.operand == 0;
        self.status.negative = self.operand.bit(7);
        self.clock.advance(1);
    }

    /**
     * No operation
     */
    async fn nop(&mut self) {
        self.clock.advance(1);
    }

    /**
     * Logical inclusive OR
     */
    async fn ora(&mut self) {
        self.a |= self.operand;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Rotate left
     */
    async fn rol(&mut self) {
        let result = (self.operand << 1).change_bit(0, self.status.carry);
        self.status.carry = self.operand.bit(7);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        self.operand = result;
        self.clock.advance(1);
    }

    /**
     * Rotate right
     */
    async fn ror(&mut self) {
        let result = (self.operand >> 1).change_bit(7, self.status.carry);
        self.status.carry = self.operand.bit(0);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        self.operand = result;
        self.clock.advance(1);
    }

    /**
     * Subtract with carry
     * Turns out that SBC(x) = ADC(!x), because of the nature of bitwise
     * addition and subtraction for two's complement numbers.
     */
    async fn sbc(&mut self) {
        let result = self.a as u16 + !self.operand as u16 + self.status.carry as u16;
        self.status.carry = result > 0xff;
        self.status.zero = result.low_byte() == 0;
        self.status.overflow = (!self.operand.bit(7) == self.a.bit(7)) && (!self.operand.bit(7) != result.bit(7));
        self.status.negative = result.bit(7);
        self.a = result.low_byte();
    }

    /**
     * LAX, similar to LDA followed by TAX
     */
    async fn lax(&mut self) {
        self.a = self.operand;
        self.x = self.operand;
        self.status.zero = self.operand == 0;
        self.status.negative = self.operand.bit(7);
    }

    /**
     * Loads memory into the accumulator register
     */
    async fn lda(&mut self) {
        self.a = self.operand;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Loads memory into the X register
     */
    async fn ldx(&mut self) {
        self.x = self.operand;
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Loads memory into the Y register
     */
    async fn ldy(&mut self) {
        self.y = self.operand;
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * BRK - Force interrupt
     */
    async fn brk(&mut self) {
        self.clock.advance(1); // Dummy read
        self.push(self.program_counter.high_byte()).await;
        self.push(self.program_counter.low_byte()).await;
        self.push(self.status.instruction_value()).await;
        self.address = 0xfffe;
        self.program_counter = Word::from_bytes(
            self.fetch().await,
            self.read().await,
        );
    }

    /**
     * JMP - Jump
     */
    async fn jmp(&mut self) {
        self.program_counter = self.address;
    }
    
    /**
     * JSR - Jump to subroutine
     */
    async fn jsr(&mut self) {
        let low_byte = self.fetch().await;
        self.clock.advance(1);
        self.push(self.program_counter.high_byte()).await;
        self.push(self.program_counter.low_byte()).await;
        let high_byte = self.fetch().await;
        self.program_counter = Word::from_bytes(low_byte, high_byte);
    }

    /**
     * PHA - Push accumulator
     */
    async fn pha(&mut self) {
        self.clock.advance(1);
        self.push(self.a).await;
    }

    /**
     * PHP - Push processor status
     */
    async fn php(&mut self) {
        self.clock.advance(1);
        self.push(self.status.instruction_value()).await;
    }

    /**
     * PLA - Pull accumulator
     */
    async fn pla(&mut self) {
        self.clock.advance(2);
        self.a = self.pull().await;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * PLP - Pull processor status
     */
    async fn plp(&mut self) {
        self.clock.advance(2);
        self.status = self.pull().await.into();
    }

    /**
     * RTI - Return from interrupt
     */
    async fn rti(&mut self) {
        self.clock.advance(2); // Dummy read and pre-increment of stack pointer
        self.status = self.pull().await.into();
        self.program_counter = u16::from_bytes(
            self.pull().await,
            self.pull().await
        );
    }

    /**
     * RTS - Return from subroutine
     */
    async fn rts(&mut self) {
        self.clock.advance(2); // Dummy read and pre-increment of stack pointer
        self.program_counter = u16::from_bytes(
            self.pull().await,
            self.pull().await
        ).wrapping_add(1);
        self.clock.advance(1);
    }

    /**
     * Transfer accumulator to X register
     */
    async fn tax(&mut self) {
        self.clock.advance(1); // Dummy read
        self.x = self.a;
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Transfer accumulator to Y register
     */
    async fn tay(&mut self) {
        self.clock.advance(1); // Dummy read
        self.y = self.a;
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * Transfer stack pointer to X register
     */
    async fn tsx(&mut self) {
        self.clock.advance(1); // Dummy read
        self.x = self.stack_pointer;
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Transfer X register to accumulator
     */
    async fn txa(&mut self) {
        self.clock.advance(1); // Dummy read
        self.a = self.x;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Transfer X register to stack pointer
     */
    async fn txs(&mut self) {
        self.clock.advance(1); // Dummy read
        self.stack_pointer = self.x;
    }

    /**
     * Transfer Y register to accumulator
     */
    async fn tya(&mut self) {
        self.clock.advance(1); // Dummy read
        self.a = self.y;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Stores the accumulator in memory
     */
    async fn sta(&mut self) {
        self.write(self.a).await;
    }

    /**
     * Stores the X register in memory
     */
    async fn stx(&mut self) {
        self.write(self.x).await;
    }

    /**
     * Stores the Y register in memory
     */
    async fn sty(&mut self) {
        self.write(self.y).await;
    }

    /**
     * Stores the bitwise AND of the X and accumulator register in memory
     */
    async fn sax(&mut self) {
        self.write(self.a & self.x).await;
    }

    /**
     * Immediate AND, followed by N being copied to C.
     * TODO: Implement tests to ensure correctness.
     */
    async fn anc(&mut self) {
        self.and().await;
        self.status.carry = self.status.negative;
    }

    /**
     * Immediate AND followed by LSR A.
     * Cannot be implemented in terms of those two instructions because of
     * timing problems, however.
     * TODO: Implement tests to ensure correctness.
     */
    async fn alr(&mut self) {
        self.a &= self.operand;
        self.status.carry = self.a.bit(0);
        self.a >>= 1;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }

    /**
     * Similar to immediate AND followed by ROR A, except for different flags:
     * - N, Z are normal
     * - C is bit 6 of the accumulator
     * - V is bit 6 xor bit 5 of the accumulator
     * TODO: Implement tests to ensure correctness.
     */
    async fn arr(&mut self) {
        self.a &= self.operand;
        self.a = (self.a >> 1).change_bit(7, self.status.carry);
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
        self.status.carry = self.a.bit(6);
        self.status.overflow = self.a.bit(6) ^ self.a.bit(5);
    }

    /**
     * Sets X to A&X minus immediate value, updating the appropriate arithmetic
     * flags.
     * TODO: Implement tests to ensure correctness.
     */
    async fn axs(&mut self) {
        let (result, carry) = (self.a & self.x).overflowing_sub(self.operand);
        self.x = result;
        self.status.negative = self.x.bit(7);
        self.status.zero = self.x == 0;
        self.status.carry = carry;
    }

    /**
     * TXA followed by immediate AND, but with one less cycle.
     * TODO: Implement tests to ensure correctness.
     */
    async fn xaa(&mut self) {
        self.a = self.x;
        self.a &= self.operand;
        self.status.zero = self.a == 0;
        self.status.negative = self.a.bit(7);
    }
}

impl<'nes, Memory: Pinout> std::fmt::Debug for Ricoh2A03<'nes, Memory> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "\n${:04x}\tA:${:02x} X:${:02x} Y:${:02x} P:{:?} SP:${:02x} CYC:{}",
            self.program_counter,
            self.a,
            self.x,
            self.y,
            self.status,
            self.stack_pointer,
            self.clock.current()
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
impl<'nes, Memory: Pinout> PartialEq for Ricoh2A03<'nes, Memory> {
    fn eq(&self, rhs: &Self) -> bool {
        self.clock == rhs.clock
        && self.status == rhs.status
        && self.program_counter == rhs.program_counter
        && self.stack_pointer == rhs.stack_pointer
        && self.a == rhs.a
        && self.x == rhs.x
        && self.y == rhs.y
    }
}

#[cfg(test)]
impl<'nes, Memory: Pinout> Eq for Ricoh2A03<'nes, Memory> {}

/**
 * Clone is needed to construct a history of CPU states.
 */
#[cfg(test)]
impl<'nes, Memory: Pinout> Clone for Ricoh2A03<'nes, Memory> {
    fn clone(&self) -> Self {
        Self {
            clock: self.clock.clone(),
            status: self.status,
            program_counter: self.program_counter,
            stack_pointer: self.stack_pointer,
            a: self.a,
            x: self.x,
            y: self.y,
            operand: self.operand,
            address: self.address,
            bus: self.bus,
        }
    }
}


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
 * Processor status is kept track of in a special 8-bit register. While the
 * flags are mostly set and checked separately, they are pushed on and pulled
 * from the stack as a single byte.
 * 
 * Since the status is pushed on and pulled from the stack only rarely, it is
 * faster to use bools for the flags, as this means that no bit twiddling is
 * required to set/clear them. Only when pushed/pulled, the bools are combined
 * the single byte that they really are.
 */
#[cfg_attr(test, derive(Copy, Clone, PartialEq, Eq))]
struct Status {
    carry: bool,
    zero: bool,
    interrupt_disable: bool,
    decimal_mode: bool,
    // Phantom bit: break flag
    // Phantom bit: unused, always 1
    overflow: bool,
    negative: bool,
}

impl Status {
    fn new() -> Self {
        Self {
            carry: false,
            zero: false,
            interrupt_disable: true,
            decimal_mode: false,
            overflow: false,
            negative: false,
        }
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
        self.carry as u8 |
        (self.zero as u8) << 1 |
        (self.interrupt_disable as u8) << 2 |
        (self.decimal_mode as u8) << 3 |
        (self.overflow as u8) << 6 |
        (self.negative as u8) << 7 |
        0x20
    }
}

impl std::convert::From<u8> for Status {
    fn from(byte: u8) -> Self {
        Self {
            carry: byte.bit(0),
            zero:  byte.bit(1),
            interrupt_disable: byte.bit(2),
            decimal_mode: byte.bit(3),
            overflow: byte.bit(6),
            negative: byte.bit(7),
        }
    }
}

impl std::fmt::Debug for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;
        f.write_char(if self.negative { 'N' } else { 'n' })?;
        f.write_char(if self.overflow { 'V' } else { 'v' })?;
        f.write_str("--")?;
        f.write_char(if self.decimal_mode { 'D' } else { 'd' })?;
        f.write_char(if self.interrupt_disable { 'I' } else { 'i' })?;
        f.write_char(if self.zero { 'Z' } else { 'z' })?;
        f.write_char(if self.carry { 'C' } else { 'c' })?;
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
        assert_eq!(status.carry, false);
        assert_eq!(status.zero, false);
        assert_eq!(status.interrupt_disable, true);
        assert_eq!(status.decimal_mode, false);
        assert_eq!(status.overflow, false);
        assert_eq!(status.negative, false);
        assert_eq!(format!("{:?}", status), "nv--dIzc ($24)");
    }

    #[test]
    fn assignment() {
        let mut status = Status::new();
        status.carry = true;
        status.zero = true;
        status.interrupt_disable = true;
        status.decimal_mode = true;
        status.overflow = true;
        status.negative = true;
        assert_eq!(status.interrupt_value(), 0b11101111);
        assert_eq!(status.instruction_value(), 0b11111111);
        assert_eq!(format!("{:?}", status), "NV--DIZC ($ef)");

        status.carry = false;
        status.zero = false;
        status.interrupt_disable = false;
        status.decimal_mode = false;
        status.overflow = false;
        status.negative = false;
        assert_eq!(format!("{:?}", status), "nv--dizc ($20)");
    }

    #[test]
    fn conversion() {
        let status: Status = 0b11111111.into();
        assert_eq!(status.interrupt_value(), 0b11101111);
        assert_eq!(status.instruction_value(), 0b11111111);
        assert_eq!(status.carry, true);
        assert_eq!(status.zero, true);
        assert_eq!(status.interrupt_disable, true);
        assert_eq!(status.decimal_mode, true);
        assert_eq!(status.overflow, true);
        assert_eq!(status.negative, true);
        assert_eq!(format!("{:?}", status), "NV--DIZC ($ef)");

        let status: Status = 0b00000000.into();
        assert_eq!(status.interrupt_value(), 0b00100000);
        assert_eq!(status.instruction_value(), 0b00110000);
        assert_eq!(status.carry, false);
        assert_eq!(status.zero, false);
        assert_eq!(status.interrupt_disable, false);
        assert_eq!(status.decimal_mode, false);
        assert_eq!(status.overflow, false);
        assert_eq!(status.negative, false);
        assert_eq!(format!("{:?}", status), "nv--dizc ($20)");
    }        
}

#[cfg(test)]
mod instruction_set {
    use super::*;
    use std::cell::Cell;
    use std::io::*;

    /**
     * For testing purposes, we here use a bus that directly writes to
     * an array, without memory mapping.
     */
    struct ArrayBus {
        data: [Cell<u8>; 0x10000],
    }

    impl ArrayBus {
        fn new() -> Self {
            Self { data: unsafe { std::mem::transmute([0u8; 0x10000]) } }
        }

        fn load_nestest(path: &std::path::Path) -> std::io::Result<Self> {
            let mut buffer = [0u8; 0x10000];
            let mut file = std::fs::File::open(path)?;
            file.seek(SeekFrom::Start(16))?;
            file.read_exact(&mut buffer[0x8000..=0xbfff])?;

            for i in 0xc000..=0xffff {
                buffer[i] = buffer[i.wrapping_sub(0x4000)];
            }

            Ok(Self { data: unsafe { std::mem::transmute(buffer) } } )
        }
    }

    impl Pinout for ArrayBus {
        fn read(&self, address: u16, _time: &Clock) -> Option<u8> {
            Some(self.data[address as usize].get())
        }

        fn write(&self, address: u16, data: u8, _time: &Clock) -> Option<()> {
            self.data[address as usize].set(data);
            Some(())
        }

        fn nmi(&self, _time: &Clock) -> Option<bool> { Some(false) }
        fn irq(&self, _time: &Clock) -> Option<bool> { Some(false) }
        fn reset(&self, _time: &Clock) -> Option<bool> { Some(false) }
    }

    #[test]
    fn cycles() {
        let bus = ArrayBus::new();
        let mut cpu = Ricoh2A03::reset(&bus);

        macro_rules! check_cycles {
            ($opcode:expr, $cycles:expr, $instruction:expr, $addressing:expr) => {{
                cpu.program_counter = 0xf0;
                cpu.x = 0;
                cpu.y = 0;

                let start = cpu.clock.current();
                futures::executor::block_on(cpu.execute($opcode));

                // Add one cycle to compensate for missing opcode read
                let cycles = cpu.clock.current() + 1 - start;
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
                cpu.status.$flag = !($value);
                check_cycles!($opcode, $cycles, $instruction, $addressing);

                cpu.status.$flag = $value;
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
        check_cycles!(0xab, 2, "XAA", "Immediate");
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
        let bus = ArrayBus::new();
        let mut cpu = Ricoh2A03::reset(&bus);

        macro_rules! check_bytes {
            ($opcode:expr, $bytes:expr, $instruction:expr, $addressing:expr) => {{
                cpu.program_counter = 0xf0;
                cpu.x = 0;
                cpu.y = 0;

                let first_byte = cpu.program_counter;
                futures::executor::block_on(cpu.execute($opcode));

                // Add one byte to compensate for missing opcode read
                let bytes = cpu.program_counter + 1 - first_byte;
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
                cpu.status.$flag = !$value;
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
        check_bytes!(0xab, 2, "XAA", "Immediate");
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
            let mut bus = ArrayBus::new();
            bus.data[0xc000] = Cell::from(0xff);
            bus
        };
        let mut cpu = Ricoh2A03::reset(&bus);
        cpu.program_counter = 0xc000;

        futures::executor::block_on(cpu.execute(0x4c));
        assert_eq!(cpu.program_counter, 0x00ff);
    }

    #[test]
    fn nestest() {
        use std::fs::File;
        use std::io::BufReader;

        let bus = ArrayBus::load_nestest(std::path::Path::new("nestest.nes")).expect("Unable to load nestest rom");
        let mut cpu = Ricoh2A03::reset(&bus);
        let nintendulator = BufReader::new(File::open("nestest.log").expect("Unable to load nestest log"));

        let mut history: Vec<(Ricoh2A03<ArrayBus>, String)> = Vec::new();
        for _ in 0..50 {
            history.push((Ricoh2A03::reset(&bus), String::new()));
        }

        for log_line in nintendulator.lines() {
            let log_line = log_line.expect("Error reading Nintendulator log line");
            let nintendulator = Ricoh2A03::from_nintendulator(&log_line);

            // Terribly inefficient, but fine, it's the easiest way to show
            // the execution history in order.
            history[0] = (cpu.clone(), log_line.clone());
            history.sort_by(|a, b| a.0.clock.current().cmp(&b.0.clock.current()));
            
            assert_eq!(cpu, nintendulator,
                "\nHistory:\n{:?}
                \nDoes not match Nintendulator log:\
                {:?} (Current)\
                {:?} (Correct)\n",
                history,
                cpu,
                nintendulator
            );

            futures::executor::block_on(cpu.step());
        }

        assert_eq!(bus.read(0x0002, &cpu.clock), Some(0x00), "Nestest failed: byte at $02 not $00, documented opcodes wrong");
        assert_eq!(bus.read(0x0003, &cpu.clock), Some(0x00), "Nestest failed: byte at $03 not $00, illegal opcodes wrong");
    }
}