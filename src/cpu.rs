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
#[derive(Clone, PartialEq, Eq)]
pub struct Ricoh2A03 {
    clock: Clock,
    status: Status,
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
}

impl std::fmt::Debug for Ricoh2A03 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,
            "\n${:04x}\tA:${:02x} X:${:02x} Y:${:02x} P:{:?} SP:${:02x} CYC:{}",
            self.program_counter,
            self.accumulator,
            self.x,
            self.y,
            self.status,
            self.stack_pointer,
            self.clock.current()
        )?;
        Ok(())
    }
}

impl Ricoh2A03 {
    pub fn new() -> Self {
        Self {
            clock: Clock::from(7), // Start-up takes 7 cycles
            program_counter: 0xc000,
            status: Status::new(),
            stack_pointer: 0xfd,
            accumulator: 0x00,
            x: 0x00,
            y: 0x00,
        }
    }

    /**
     * For testing purposes, we also allow a CPU to be constructed from a
     * Nintendulator log entry.
     */
    #[cfg(test)]
    fn from_nintendulator(log: &str) -> Self {
        let mut state = log.split_whitespace();
        let program_counter: u16 = u16::from_str_radix(state.next().unwrap(), 16).unwrap();
        let cycle: u64 = u64::from_str_radix(state.next_back().unwrap().strip_prefix("CYC:").unwrap(), 10).unwrap();

        let mut skip = state.next().unwrap().strip_prefix("A:");
        while skip.is_none() {
            skip = state.next().unwrap().strip_prefix("A:");
        }

        let accumulator = u8::from_str_radix(skip.unwrap(), 16).unwrap();
        let x = u8::from_str_radix(state.next().unwrap().strip_prefix("X:").unwrap(), 16).unwrap();
        let y = u8::from_str_radix(state.next().unwrap().strip_prefix("Y:").unwrap(), 16).unwrap();
        let status = u8::from_str_radix(state.next().unwrap().strip_prefix("P:").unwrap(), 16).unwrap();
        let stack_pointer = u8::from_str_radix(state.next().unwrap().strip_prefix("SP:").unwrap(), 16).unwrap();

        Self {
            clock: Clock::from(cycle),
            program_counter,
            status: Status::from(status),
            stack_pointer,
            accumulator,
            x,
            y,
        }
    }

    pub async fn run(&mut self, bus: &impl Bus) {
        loop {
            self.step(bus).await;
        }
    }

    async fn step(&mut self, bus: &impl Bus) {
        let opcode = self.fetch(bus).await;
        self.execute(bus, opcode).await;
    }

    /**
     * Executes the instruction associated with the given opcode.
     * Implemented as a combination of macros for the addressing modes and
     * operation types.
     */
    async fn execute(&mut self, bus: &impl Bus, opcode: u8) {
        /**
         * All branch instructions follow a similar pattern, so we use a macro
         * to generate them.
         */
        macro_rules! branch {
            ($flag:ident, $value:expr) => {{
                let offset = self.fetch(bus).await;
                if self.status.$flag == $value {
                    self.branch(offset);
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
         * Read instructions store the result of their operation only within
         * the CPU itself and do not write it elsewhere.
         * Depending on the addressing mode, a "correction cycle" must be
         * applied sometimes, when a page crossing must be accounted for.
         */
        macro_rules! read {
            ($instruction:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await as u16;
                let operand = self.read(bus, address).await;
                self.$instruction(operand);
            }};
        }

        /**
         * Modify instructions write the result of an operation back to memory
         * in some way. This can be the memory bus, or the accumulator,
         * depending on the addressing mode.
         */
        macro_rules! modify {
            ($instruction:ident, accumulator) => {{
                let operand = self.accumulator;
                self.clock.advance(1);
                self.accumulator = self.$instruction(operand);
            }};

            ($instruction:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await as u16;
                let operand = self.read(bus, address).await;
                let result = self.$instruction(operand);
                self.clock.advance(1);
                self.write(bus, address, result).await;
            }};
        }

        /**
         * Some instructions operate on an address.
         */
        macro_rules! address {
            ($instruction:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await;
                self.$instruction(address);
            }}
        }

        /**
         * Store register into memory
         * The unofficial opcode SAX stores the bitwise AND of the accumulator
         * and the X index register.
         */
        macro_rules! store {
            (a & x, $addressing:ident) => {{
                let address = self.$addressing(bus).await as u16;
                self.write(bus, address, self.accumulator & self.x).await;
            }};

            ($register:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await as u16;
                self.write(bus, address, self.$register).await;
            }};
        }

        /**
         * Transfer registers
         */
        macro_rules! transfer {
            // If a value is stored into the stack pointer, the status flags
            // are not updated.
            ($from:ident, stack_pointer) => {{
                self.clock.advance(1); // Dummy read
                self.stack_pointer = self.$from;
            }};
            
            ($from:ident, $into:ident) => {{
                self.clock.advance(1); // Dummy read
                self.$into = self.$from;
                self.status.zero = self.$into == 0;
                self.status.negative = self.$into.bit(7);
            }};
        }

        /**
         * Some operations have an implied addressing mode.
         * Although they do not use the instruction operand, the 6502 still
         * uses a cycle to read it, after which it is directly discarded.
         */
        macro_rules! implied {
            ($instruction:ident) => {{
                self.clock.advance(1);
                self.$instruction();
            }}
        }

        /**
         * System instructions have an implied addressing mode, but must
         * access the system bus, and are therefore implemented separately for
         * now, since this means they must be asynchronous.
         */
        macro_rules! system {
            ($instruction:ident) => {{
                self.$instruction(bus).await;
            }}
        }

        /**
         * Among undocumented opcodes, some NOP instructions exist with
         * specific addressing modes. These do nothing but reading the value
         * of the operand, after which it is discarded.
         */
        macro_rules! noop {
            ($addressing:ident) => {{
                let address = self.$addressing(bus).await as u16;
                let _operand = self.read(bus, address).await;
            }};
        }

        /**
         * Some undocumented opcodes are essentially combinations of two
         * documented instructions, one being read-modify-write, followed by
         * a read instruction.
         */
        macro_rules! combine {
            ($write:ident, $read:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await as u16;
                let operand = self.read(bus, address).await;
                let result = self.$write(operand);
                self.clock.advance(1);
                self.write(bus, address, result).await;
                self.$read(result);
            }}
        }

        match opcode {
            0x00 => system!(brk),
            0x01 => read!(ora, indexed_indirect),
            0x02 => implied!(nop), // In reality, halts the machine
            0x03 => combine!(asl, ora, indexed_indirect),
            0x04 => noop!(zeropage),
            0x05 => read!(ora, zeropage),
            0x06 => modify!(asl, zeropage),
            0x07 => combine!(asl, ora, zeropage),
            0x08 => system!(php),
            0x09 => read!(ora, immediate),
            0x0a => modify!(asl, accumulator),
            0x0b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x0c => noop!(absolute),
            0x0d => read!(ora, absolute),
            0x0e => modify!(asl, absolute),
            0x0f => combine!(asl, ora, absolute),
            0x10 => branch!(negative, false),
            0x11 => read!(ora, indirect_indexed_cross),
            0x12 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x13 => combine!(asl, ora, indirect_indexed_cross),
            0x14 => noop!(zeropage_x),
            0x15 => read!(ora, zeropage_x),
            0x16 => modify!(asl, zeropage_x),
            0x17 => combine!(asl, ora, zeropage_x),
            0x18 => set!(carry, false),
            0x19 => read!(ora, absolute_y_cross),
            0x1a => implied!(nop),
            0x1b => combine!(asl, ora, absolute_y_cross),
            0x1c => noop!(absolute_x_cross),
            0x1d => read!(ora, absolute_x_cross),
            0x1e => modify!(asl, absolute_x),
            0x1f => combine!(asl, ora, absolute_x),
            0x20 => system!(jsr),
            0x21 => read!(and, indexed_indirect),
            0x22 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x23 => combine!(rol, and, indexed_indirect),
            0x24 => read!(bit, zeropage),
            0x25 => read!(and, zeropage),
            0x26 => modify!(rol, zeropage),
            0x27 => combine!(rol, and, zeropage),
            0x28 => system!(plp),
            0x29 => read!(and, immediate),
            0x2a => modify!(rol, accumulator),
            0x2b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x2c => read!(bit, absolute),
            0x2d => read!(and, absolute),
            0x2e => modify!(rol, absolute),
            0x2f => combine!(rol, and, absolute),
            0x30 => branch!(negative, true),
            0x31 => read!(and, indirect_indexed_cross),
            0x32 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x33 => combine!(rol, and, indirect_indexed_cross),
            0x34 => noop!(zeropage_x),
            0x35 => read!(and, zeropage_x),
            0x36 => modify!(rol, zeropage_x),
            0x37 => combine!(rol, and, zeropage_x),
            0x38 => set!(carry, true),
            0x39 => read!(and, absolute_y_cross),
            0x3a => implied!(nop),
            0x3b => combine!(rol, and, absolute_y_cross),
            0x3c => noop!(absolute_x_cross),
            0x3d => read!(and, absolute_x_cross),
            0x3e => modify!(rol, absolute_x),
            0x3f => combine!(rol, and, absolute_x),
            0x40 => system!(rti),
            0x41 => read!(eor, indexed_indirect),
            0x42 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x43 => combine!(lsr, eor, indexed_indirect),
            0x44 => noop!(zeropage),
            0x45 => read!(eor, zeropage),
            0x46 => modify!(lsr, zeropage),
            0x47 => combine!(lsr, eor, zeropage),
            0x48 => system!(pha),
            0x49 => read!(eor, immediate),
            0x4a => modify!(lsr, accumulator),
            0x4b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x4c => address!(jmp, absolute),
            0x4d => read!(eor, absolute),
            0x4e => modify!(lsr, absolute),
            0x4f => combine!(lsr, eor, absolute),
            0x50 => branch!(overflow, false),
            0x51 => read!(eor, indirect_indexed_cross),
            0x52 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x53 => combine!(lsr, eor, indirect_indexed_cross),
            0x54 => noop!(zeropage_x),
            0x55 => read!(eor, zeropage_x),
            0x56 => modify!(lsr, zeropage_x),
            0x57 => combine!(lsr, eor, zeropage_x),
            0x58 => set!(interrupt_disable, false),
            0x59 => read!(eor, absolute_y_cross),
            0x5a => implied!(nop),
            0x5b => combine!(lsr, eor, absolute_y_cross),
            0x5c => noop!(absolute_x_cross),
            0x5d => read!(eor, absolute_x_cross),
            0x5e => modify!(lsr, absolute_x),
            0x5f => combine!(lsr, eor, absolute_x),
            0x60 => system!(rts),
            0x61 => read!(adc, indexed_indirect),
            0x62 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x63 => combine!(ror, adc, indexed_indirect),
            0x64 => noop!(zeropage),
            0x65 => read!(adc, zeropage),
            0x66 => modify!(ror, zeropage),
            0x67 => combine!(ror, adc, zeropage),
            0x68 => system!(pla),
            0x69 => read!(adc, immediate),
            0x6a => modify!(ror, accumulator),
            0x6b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x6c => address!(jmp, indirect),
            0x6d => read!(adc, absolute),
            0x6e => modify!(ror, absolute),
            0x6f => combine!(ror, adc, absolute),
            0x70 => branch!(overflow, true),
            0x71 => read!(adc, indirect_indexed_cross),
            0x72 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x73 => combine!(ror, adc, indirect_indexed_cross),
            0x74 => noop!(zeropage_x),
            0x75 => read!(adc, zeropage_x),
            0x76 => modify!(ror, zeropage_x),
            0x77 => combine!(ror, adc, zeropage_x),
            0x78 => set!(interrupt_disable, true),
            0x79 => read!(adc, absolute_y_cross),
            0x7a => implied!(nop),
            0x7b => combine!(ror, adc, absolute_y_cross),
            0x7c => noop!(absolute_x_cross),
            0x7d => read!(adc, absolute_x_cross),
            0x7e => modify!(ror, absolute_x),
            0x7f => combine!(ror, adc, absolute_x),
            0x80 => noop!(immediate),
            0x81 => store!(accumulator, indexed_indirect),
            0x82 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x83 => store!(a & x, indexed_indirect),
            0x84 => store!(y, zeropage),
            0x85 => store!(accumulator, zeropage),
            0x86 => store!(x, zeropage),
            0x87 => store!(a & x, zeropage),
            0x88 => implied!(dey),
            0x89 => noop!(immediate),
            0x8a => transfer!(x, accumulator),
            0x8b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x8c => store!(y, absolute),
            0x8d => store!(accumulator, absolute),
            0x8e => store!(x, absolute),
            0x8f => store!(a & x, absolute),
            0x90 => branch!(carry, false),
            0x91 => store!(accumulator, indirect_indexed),
            0x92 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x93 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x94 => store!(y, zeropage_x),
            0x95 => store!(accumulator, zeropage_x),
            0x96 => store!(x, zeropage_y),
            0x97 => store!(a & x, zeropage_y),
            0x98 => transfer!(y, accumulator),
            0x99 => store!(accumulator, absolute_y),
            0x9a => transfer!(x, stack_pointer),
            0x9b => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x9c => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x9d => store!(accumulator, absolute_x),
            0x9e => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0x9f => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xa0 => read!(ldy, immediate),
            0xa1 => read!(lda, indexed_indirect),
            0xa2 => read!(ldx, immediate),
            0xa3 => read!(lax, indexed_indirect),
            0xa4 => read!(ldy, zeropage),
            0xa5 => read!(lda, zeropage),
            0xa6 => read!(ldx, zeropage),
            0xa7 => read!(lax, zeropage),
            0xa8 => transfer!(accumulator, y),
            0xa9 => read!(lda, immediate),
            0xaa => transfer!(accumulator, x),
            0xab => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xac => read!(ldy, absolute),
            0xad => read!(lda, absolute),
            0xae => read!(ldx, absolute),
            0xaf => read!(lax, absolute),
            0xb0 => branch!(carry, true),
            0xb1 => read!(lda, indirect_indexed_cross),
            0xb2 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xb3 => read!(lax, indirect_indexed_cross),
            0xb4 => read!(ldy, zeropage_x),
            0xb5 => read!(lda, zeropage_x),
            0xb6 => read!(ldx, zeropage_y),
            0xb7 => read!(lax, zeropage_y),
            0xb8 => set!(overflow, false),
            0xb9 => read!(lda, absolute_y_cross),
            0xba => transfer!(stack_pointer, x),
            0xbb => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xbc => read!(ldy, absolute_x_cross),
            0xbd => read!(lda, absolute_x_cross),
            0xbe => read!(ldx, absolute_y_cross),
            0xbf => read!(lax, absolute_y_cross),
            0xc0 => read!(cpy, immediate),
            0xc1 => read!(cmp, indexed_indirect),
            0xc2 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xc3 => combine!(dec, cmp, indexed_indirect),
            0xc4 => read!(cpy, zeropage),
            0xc5 => read!(cmp, zeropage),
            0xc6 => modify!(dec, zeropage),
            0xc7 => combine!(dec, cmp, zeropage),
            0xc8 => implied!(iny),
            0xc9 => read!(cmp, immediate),
            0xca => implied!(dex),
            0xcb => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xcc => read!(cpy, absolute),
            0xcd => read!(cmp, absolute),
            0xce => modify!(dec, absolute),
            0xcf => combine!(dec, cmp, absolute),
            0xd0 => branch!(zero, false),
            0xd1 => read!(cmp, indirect_indexed_cross),
            0xd2 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xd3 => combine!(dec, cmp, indirect_indexed_cross),
            0xd4 => noop!(zeropage_x),
            0xd5 => read!(cmp, zeropage_x),
            0xd6 => modify!(dec, zeropage_x),
            0xd7 => combine!(dec, cmp, zeropage_x),
            0xd8 => set!(decimal_mode, false),
            0xd9 => read!(cmp, absolute_y_cross),
            0xda => implied!(nop),
            0xdb => combine!(dec, cmp, absolute_y_cross),
            0xdc => noop!(absolute_x_cross),
            0xdd => read!(cmp, absolute_x_cross),
            0xde => modify!(dec, absolute_x),
            0xdf => combine!(dec, cmp, absolute_x),
            0xe0 => read!(cpx, immediate),
            0xe1 => read!(sbc, indexed_indirect),
            0xe2 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xe3 => combine!(inc, sbc, indexed_indirect),
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
            0xf1 => read!(sbc, indirect_indexed_cross),
            0xf2 => unimplemented!("Encountered unimplemented opcode ${:x}, CPU state: {:?}", opcode, self),
            0xf3 => combine!(inc, sbc, indirect_indexed_cross),
            0xf4 => noop!(zeropage_x),
            0xf5 => read!(sbc, zeropage_x),
            0xf6 => modify!(inc, zeropage_x),
            0xf7 => combine!(inc, sbc, zeropage_x),
            0xf8 => set!(decimal_mode, true),
            0xf9 => read!(sbc, absolute_y_cross),
            0xfa => implied!(nop),
            0xfb => combine!(inc, sbc, absolute_y_cross),
            0xfc => noop!(absolute_x_cross),
            0xfd => read!(sbc, absolute_x_cross),
            0xfe => modify!(inc, absolute_x),
            0xff => combine!(inc, sbc, absolute_x),
        }
    }

    /**
     * Reads from the bus, taking one cycle.
     */
    async fn read(&self, bus: &impl Bus, address: u16) -> u8 {
        loop {
            match bus.read(address, &self.clock) {
                Some(data) => {
                    self.clock.advance(1); break data
                },
                None => yields().await,
            }
        }
    }

    /**
     * Writes to the bus, taking one cycle.
     */
    async fn write(&self, bus: &impl Bus, address: u16, data: u8) {
        while let None = bus.write(address, data, &self.clock) {
            yields().await;
        }
        self.clock.advance(1);
    }

    /**
     * Some instructions require an extra cycle if a page boundary is crossed,
     * to adjust the high byte..
     */
    fn cross(&mut self, old_address: u16, new_address: u16) {
        if old_address.high_byte() != new_address.high_byte() {
            self.clock.advance(1);
        }
    }

    /**
     * Pushes a value onto the stack. Takes one cycle.
     */
    async fn push(&mut self, bus: &impl Bus, data: u8) {
        let address = 0x0100 | self.stack_pointer as u16;
        self.write(bus, address, data).await;
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    /**
     * Pulls a value from the stack. Takes two cycles, or one if the previous
     * operation was also a pull. As such, the second cycle must be added
     * manually.
     */
    async fn pull(&mut self, bus: &impl Bus) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let address = 0x0100 | self.stack_pointer as u16;
        let result = self.read(bus, address).await;
        result
    }

    /**
     * Reads a byte from the location of the program counter, and increments
     * the program counter afterwards. Takes one cycle, because of the read.
     */
    async fn fetch(&mut self, bus: &impl Bus) -> u8 {
        let result = self.read(bus, self.program_counter).await;
        self.program_counter = self.program_counter.wrapping_add(1);
        result
    }

    /**
     * Takes a branch, by moving the program counter by the given offset.
     * The offset is computed as the two's complement signed interpretation of
     * the given offset byte.
     * Takes one cycle, and one more if the program counter moves to a new
     * page.
     */
    fn branch(&mut self, offset: u8) {
        self.clock.advance(1);
        let old_address = self.program_counter;
        let offset = offset as i8;
        self.program_counter = self.program_counter.wrapping_add(offset as u16);
        self.cross(old_address, self.program_counter);
    }

    /**
     * Immediate addressing loads the byte straight after the opcode as
     * operand.
     */
    async fn immediate(&mut self, _bus: &impl Bus) -> u16 {
        let address = self.program_counter;
        self.program_counter = self.program_counter.wrapping_add(1);
        address
    }

    /**
     * Absolute addressing loads the two bytes after the opcode into a 16-bit
     * word that it uses as effective address.
     */
    async fn absolute(&mut self, bus: &impl Bus) -> u16 {
        let low_byte = self.fetch(bus).await;
        let high_byte = self.fetch(bus).await;
        Word::from_bytes(low_byte, high_byte)
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     */
    async fn absolute_x_cross(&mut self, bus: &impl Bus) -> u16 {
        let address = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.x as u16);
        self.cross(address, effective_address);
        effective_address
    }

    async fn absolute_x(&mut self, bus: &impl Bus) -> u16 {
        let address = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.x as u16);
        self.clock.advance(1);
        effective_address
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     */
    async fn absolute_y(&mut self, bus: &impl Bus) -> u16 {
        let address = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.y as u16);
        self.clock.advance(1);
        effective_address
    }

    async fn absolute_y_cross(&mut self, bus: &impl Bus) -> u16 {
        let address = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.y as u16);
        self.cross(address, effective_address);
        effective_address
    }

    /**
     * Zeropage addressing operates on the address obtained by interpreting the
     * immediate operand as an address.
     */
    async fn zeropage(&mut self, bus: &impl Bus) -> u8 {
        self.fetch(bus).await
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by adding
     * an index register to the immediate operand, and interpreting that as an
     * address.
     * Note: the high byte is always zero, page boundary crossings are not
     * possible.
     */
    async fn zeropage_x(&mut self, bus: &impl Bus) -> u8 {
        let address = self.fetch(bus).await.wrapping_add(self.x);
        self.clock.advance(1);
        address
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by adding
     * an index register to the immediate operand, and interpreting that as an
     * address.
     * Note: the high byte is always zero, page boundary crossings are not
     * possible.
     */
    async fn zeropage_y(&mut self, bus: &impl Bus) -> u8 {
        let address = self.fetch(bus).await.wrapping_add(self.y);
        self.clock.advance(1);
        address
    }

    /**
     * Indirect addressing uses a 16-bit operand as address from which it reads
     * the actual address to operate on.
     * Is bugged in the sense that indirect vectors on a page boundary do not
     * correctly cross that page boundary, e.g. $00ff reads from ($00ff, $0000)
     * instead of ($00ff, $0100).
     */
    async fn indirect(&mut self, bus: &impl Bus) -> u16 {
        let low_address = self.absolute(bus).await;
        let high_address = Word::from_bytes(
            low_address.low_byte().wrapping_add(1),
            low_address.high_byte()
        );
        let low_byte = self.read(bus, low_address).await;
        let high_byte = self.read(bus, high_address).await;
        Word::from_bytes(low_byte, high_byte)
    }

    /**
     * Indexed indirect reads the 16-bit target address from the memory found
     * at a zero page X-indexed address.
     */
    async fn indexed_indirect(&mut self, bus: &impl Bus) -> u16 {
        let address = self.zeropage_x(bus).await;
        let low_byte = self.read(bus, address as u16).await;
        let high_byte = self.read(bus, address.wrapping_add(1) as u16).await;
        Word::from_bytes(low_byte, high_byte)
    }

    /**
     * Indirect indexed adds the Y register to the 16-bit address read from the
     * location found using zero page indexing.
     * If a page boundary is crossed when adding Y, an extra cycle might be
     * used, depending on the instruction.
     */
    async fn indirect_indexed(&mut self, bus: &impl Bus) -> u16 {
        let zeropage_address = self.zeropage(bus).await;
        let low_byte = self.read(bus, zeropage_address as u16).await;
        let high_byte = self.read(bus, zeropage_address.wrapping_add(1) as u16).await;
        let address = u16::from_bytes(low_byte, high_byte);
        let effective_address = address.wrapping_add(self.y as u16);
        self.clock.advance(1);
        effective_address
    }

    async fn indirect_indexed_cross(&mut self, bus: &impl Bus) -> u16 {
        let zeropage_address = self.zeropage(bus).await;
        let low_byte = self.read(bus, zeropage_address as u16).await;
        let high_byte = self.read(bus, zeropage_address.wrapping_add(1) as u16).await;
        let address = u16::from_bytes(low_byte, high_byte);
        let effective_address = address.wrapping_add(self.y as u16);
        self.cross(address, effective_address);
        effective_address
    }


    

    /**
     * Logical AND of the accumulator with a byte of memory. The result is
     * stored in the accumulator.
     */
    fn and(&mut self, operand: u8) {
        self.accumulator &= operand;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
    }

    /**
     * Add with carry.
     */
    fn adc(&mut self, operand: u8) {
        let result = self.accumulator as u16 + operand as u16 + self.status.carry as u16;
        self.status.carry = result > 0xff;
        self.status.zero = result.low_byte() == 0;
        self.status.overflow = (operand.bit(7) == self.accumulator.bit(7)) && (operand.bit(7) != result.bit(7));
        self.status.negative = result.bit(7);
        self.accumulator = result.low_byte();
    }

    /**
     * Arithmetic shift left.
     */
    fn asl(&mut self, operand: u8) -> u8 {
        self.status.carry = operand.bit(7);
        let result = operand << 1;
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * Bit test
     */
    fn bit(&mut self, operand: u8) {
        let result = self.accumulator & operand;
        self.status.zero = result == 0;
        self.status.overflow = operand.bit(6);
        self.status.negative = operand.bit(7);
    }

    /**
     * Compare accumulator with memory
     */
    fn cmp(&mut self, operand: u8) {
        let result = self.accumulator.wrapping_sub(operand);
        self.status.zero = result == 0;
        self.status.carry = self.accumulator >= operand;
        self.status.negative = result.bit(7);
    }

    /**
     * Compare X register with memory
     */
    fn cpx(&mut self, operand: u8) {
        let result = self.x.wrapping_sub(operand);
        self.status.zero = result == 0;
        self.status.carry = self.x >= operand;
        self.status.negative = result.bit(7);
    }

    /**
     * Compare Y register with memory
     */
    fn cpy(&mut self, operand: u8) {
        let result = self.y.wrapping_sub(operand);
        self.status.zero = result == 0;
        self.status.carry = self.y >= operand;
        self.status.negative = result.bit(7);
    }

    /**
     * Decrement memory
     */
    fn dec(&mut self, operand: u8) -> u8 {
        let result = operand.wrapping_sub(1);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * Decrement X register
     */
    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Decrement Y register
     */
    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * Exclusive OR
     */
    fn eor(&mut self, operand: u8) {
        self.accumulator ^= operand;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
    }

    /**
     * Increment memory
     */
    fn inc(&mut self, operand: u8) -> u8 {
        let result = operand.wrapping_add(1);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * Increment X register
     */
    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Increment Y register
     */
    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * Jump to address
     */
    fn jmp(&mut self, address: u16) {
        self.program_counter = address;
    }

    /**
     * Jump to subroutine
     */
    async fn jsr(&mut self, bus: &impl Bus) {
        let address = self.absolute(bus).await;
        self.program_counter = self.program_counter.wrapping_sub(1);
        self.push(bus, self.program_counter.high_byte()).await;
        self.push(bus, self.program_counter.low_byte()).await;
        self.clock.advance(1);
        self.program_counter = address;
    }

    /**
     * Load accumulator
     */
    fn lda(&mut self, operand: u8) {
        self.accumulator = operand;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
    }

    /**
     * Load X register
     */
    fn ldx(&mut self, operand: u8) {
        self.x = operand;
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Load Y register
     */
    fn ldy(&mut self, operand: u8) {
        self.y = operand;
        self.status.zero = self.y == 0;
        self.status.negative = self.y.bit(7);
    }

    /**
     * Logical shift right
     */
    fn lsr(&mut self, operand: u8) -> u8 {
        self.status.carry = operand.bit(0);
        let result = operand >> 1;
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * No operation
     */
    fn nop(&mut self) {}

    /**
     * Logical inclusive OR
     */
    fn ora(&mut self, operand: u8) {
        self.accumulator |= operand;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
    }

    /**
     * Push accumulator
     */
    async fn pha(&mut self, bus: &impl Bus) {
        self.clock.advance(1);
        self.push(bus, self.accumulator).await;
    }

    /**
     * Push processor status
     */
    async fn php(&mut self, bus: &impl Bus) {
        self.clock.advance(1);
        self.push(bus, self.status.instruction_value()).await;
    }

    /**
     * Pull accumulator
     */
    async fn pla(&mut self, bus: &impl Bus) {
        self.clock.advance(2);
        self.accumulator = self.pull(bus).await;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
    }

    /**
     * Pull processor status
     */
    async fn plp(&mut self, bus: &impl Bus) {
        self.clock.advance(2);
        self.status = Status::from(self.pull(bus).await);
    }

    /**
     * Rotate left
     */
    fn rol(&mut self, operand: u8) -> u8 {
        let result = (operand << 1).change_bit(0, self.status.carry);
        self.status.carry = operand.bit(7);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * Rotate right
     */
    fn ror(&mut self, operand: u8) -> u8 {
        let result = (operand >> 1).change_bit(7, self.status.carry);
        self.status.carry = operand.bit(0);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * Return from interrupt
     */
    async fn rti(&mut self, bus: &impl Bus) {
        self.clock.advance(2); // Dummy read and pre-decrement of stack pointer
        self.status = Status::from(self.pull(bus).await);
        self.program_counter = Word::from_bytes(
            self.pull(bus).await,
            self.pull(bus).await
        );
    }

    /**
     * Return from subroutine
     */
    async fn rts(&mut self, bus: &impl Bus) {
        self.clock.advance(2); // Dummy read and pre-increment of stack pointer
        self.program_counter = <u16 as Word>::from_bytes(
            self.pull(bus).await,
            self.pull(bus).await
        ).wrapping_add(1);
        self.clock.advance(1);
    }

    /**
     * Subtract with carry
     * Turns out that SBC(x) = ADC(!x), because of the nature of bitwise
     * addition and subtraction for two's complement numbers.
     */
    fn sbc(&mut self, operand: u8) {
        self.adc(!operand);
    }

    /**
     * Force interrupt
     */
    async fn brk(&mut self, bus: &impl Bus) {
        self.clock.advance(1); // Dummy read
        self.push(bus, self.program_counter.high_byte()).await;
        self.push(bus, self.program_counter.low_byte()).await;
        self.push(bus, self.status.instruction_value()).await;
        self.program_counter = Word::from_bytes(
            self.read(bus, 0xfffe).await,
            self.read(bus, 0xffff).await
        );
    }

    /**
     * LAX, similar to LDA followed by TAX
     */
    fn lax(&mut self, operand: u8) {
        self.accumulator = operand;
        self.x = operand;
        self.status.zero = operand == 0;
        self.status.negative = operand.bit(7);
    }
}


/**
 * Any address bus used with the Ricoh 2A03 must implement this trait to allow
 * for interoperability. Corresponds with the CPU pin out, conceptually.
 * 
 * Functions return an empty Option only when the addressed memory or device is
 * not yet ready, i.e. if some other device must be emulated further in time
 * first.
 */
pub trait Bus {
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
#[derive(Copy, Clone, PartialEq, Eq)]
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
mod test {
    use super::*;

    mod instructions {
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

        impl Bus for ArrayBus {
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
            let mut cpu = Ricoh2A03::new();
            let bus = ArrayBus::new();

            macro_rules! check_cycles {
                ($opcode:expr, $cycles:expr, $instruction:expr, $addressing:expr) => {{
                    cpu.program_counter = 0xf0;
                    cpu.x = 0;
                    cpu.y = 0;

                    let start = cpu.clock.current();
                    futures::executor::block_on(cpu.execute(&bus, $opcode));

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
            check_cycles!(0x81, 6, "STA", "(Indirect,X)");
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
            check_cycles!(0xc4, 3, "CPY", "Zeropage");
            check_cycles!(0xc5, 3, "CMP", "Zeropage");
            check_cycles!(0xc6, 5, "DEC", "Zeropage");
            check_cycles!(0xc8, 2, "INY", "Implied");
            check_cycles!(0xc9, 2, "CMP", "Immediate");
            check_cycles!(0xca, 2, "DEX", "Implied");
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
            let mut cpu = Ricoh2A03::new();
            let bus = ArrayBus::new();

            macro_rules! check_bytes {
                ($opcode:expr, $bytes:expr, $instruction:expr, $addressing:expr) => {{
                    cpu.program_counter = 0xf0;
                    cpu.x = 0;
                    cpu.y = 0;

                    let first_byte = cpu.program_counter;
                    futures::executor::block_on(cpu.execute(&bus, $opcode));

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
            check_bytes!(0x81, 2, "STA", "(Indirect,X)");
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
            check_bytes!(0xc4, 2, "CPY", "Zeropage");
            check_bytes!(0xc5, 2, "CMP", "Zeropage");
            check_bytes!(0xc6, 2, "DEC", "Zeropage");
            check_bytes!(0xc8, 1, "INY", "Implied");
            check_bytes!(0xc9, 2, "CMP", "Immediate");
            check_bytes!(0xca, 1, "DEX", "Implied");
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
            let mut cpu = Ricoh2A03::new();
            let mut bus = ArrayBus::new();
            bus.data[0xc000] = Cell::from(0xff);
            cpu.program_counter = 0xc000;

            futures::executor::block_on(cpu.execute(&bus, 0x4c));
            assert_eq!(cpu.program_counter, 0x00ff);
        }

        #[test]
        fn nestest() {
            use std::fs::File;
            use std::io::BufReader;

            let mut cpu = Ricoh2A03::new();
            let bus = ArrayBus::load_nestest(std::path::Path::new("nestest.nes")).expect("Unable to load nestest rom");
            let nintendulator = BufReader::new(File::open("nestest.log").expect("Unable to load nestest log"));

            let mut history: Vec<(Ricoh2A03, String)> = Vec::new();
            for _ in 0..50 {
                history.push((Ricoh2A03::new(), String::new()));
            }

            for instruction in nintendulator.lines() {
                let instruction = instruction.expect("Error reading Nintendulator log line");
                let nintendulator = Ricoh2A03::from_nintendulator(&instruction);

                // Terribly inefficient, but fine, it's the easiest way to show
                // the execution history in order.
                history[0] = (cpu.clone(), instruction.clone());
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

                futures::executor::block_on(cpu.step(&bus));
            }

            assert_eq!(bus.read(0x0002, &cpu.clock), Some(0x00), "Nestest failed: byte at $02 not $00, documented opcodes wrong");
            assert_eq!(bus.read(0x0003, &cpu.clock), Some(0x00), "Nestest failed: byte at $03 not $00, illegal opcodes wrong");
        }
    }

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
}