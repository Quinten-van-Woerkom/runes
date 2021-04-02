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
#[derive(Debug)]
pub struct Ricoh2A03 {
    clock: Clock,
    status: Status,
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
}


impl Ricoh2A03 {
    pub fn new() -> Self {
        Self {
            clock: Clock::new(),
            program_counter: 0xc000,
            status: Status::new(),
            stack_pointer: 0xfd,
            accumulator: 0x00,
            x: 0x00,
            y: 0x00,
        }
    }

    pub async fn run(&mut self, bus: &impl Bus) {
        loop {
            let opcode = self.fetch(bus).await;
            self.execute(bus, opcode).await;
        }
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
                let address = self.$addressing(bus).await;
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
                let address = self.$addressing(bus).await;
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
         */
        macro_rules! store {
            ($register:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await;
                self.write(bus, address, self.$register).await;
            }}
        }

        /**
         * Transfer registers
         */
        macro_rules! transfer {
            ($from:ident, $into:ident) => {{
                self.clock.advance(1); // Dummy read
                self.$into = self.$from;
                self.status.zero = self.$into == 0;
                self.status.negative = self.$into.bit(7);
            }}
        }

        /**
         * Jump to subroutine.
         */
        macro_rules! jsr {
            ($addressing:ident) => {{
                let address = self.$addressing(bus).await;
                self.push(bus, self.program_counter.high_byte()).await;
                self.push(bus, self.program_counter.low_byte()).await;
                self.clock.advance(1);
                self.program_counter = address;
            }}
        }

        match opcode {
            0x00 => self.brk(bus).await,
            0x01 => read!(ora, indexed_indirect),
            0x05 => read!(ora, zeropage),
            0x06 => modify!(asl, zeropage),
            0x08 => self.php(bus).await,
            0x09 => read!(ora, immediate),
            0x0a => modify!(asl, accumulator),
            0x0d => read!(ora, absolute),
            0x0e => modify!(asl, absolute),
            0x10 => branch!(negative, false),
            0x11 => read!(ora, indirect_indexed_cross),
            0x15 => read!(ora, zeropage_x),
            0x16 => modify!(asl, zeropage_x),
            0x18 => set!(carry, false),
            0x19 => read!(ora, absolute_y_cross),
            0x1d => read!(ora, absolute_x_cross),
            0x1e => modify!(asl, absolute_x),
            0x20 => jsr!(absolute),
            0x21 => read!(and, indexed_indirect),
            0x24 => read!(bit, zeropage),
            0x25 => read!(and, zeropage),
            0x26 => modify!(rol, zeropage),
            0x28 => self.plp(bus).await,
            0x29 => read!(and, immediate),
            0x2a => modify!(rol, accumulator),
            0x2c => read!(bit, absolute),
            0x2d => read!(and, absolute),
            0x2e => modify!(rol, absolute),
            0x30 => branch!(negative, true),
            0x31 => read!(and, indirect_indexed_cross),
            0x35 => read!(and, zeropage_x),
            0x36 => modify!(rol, zeropage_x),
            0x38 => set!(carry, true),
            0x39 => read!(and, absolute_y_cross),
            0x3d => read!(and, absolute_x_cross),
            0x3e => modify!(rol, absolute_x),
            0x40 => self.rti(bus).await,
            0x41 => read!(eor, indexed_indirect),
            0x45 => read!(eor, zeropage),
            0x46 => modify!(lsr, zeropage),
            0x48 => self.pha(bus).await,
            0x49 => read!(eor, immediate),
            0x4a => modify!(lsr, accumulator),
            0x4c => address!(jmp, absolute),
            0x4d => read!(eor, absolute),
            0x4e => modify!(lsr, absolute),
            0x50 => branch!(overflow, false),
            0x51 => read!(eor, indirect_indexed_cross),
            0x55 => read!(eor, zeropage_x),
            0x56 => modify!(lsr, zeropage_x),
            0x58 => set!(interrupt_disable, false),
            0x59 => read!(eor, absolute_y_cross),
            0x5d => read!(eor, absolute_x_cross),
            0x5e => modify!(lsr, absolute_x),
            0x60 => self.rts(bus).await,
            0x61 => read!(adc, indexed_indirect),
            0x65 => read!(adc, zeropage),
            0x66 => modify!(ror, zeropage),
            0x68 => self.pla(bus).await,
            0x69 => read!(adc, immediate),
            0x6a => modify!(ror, accumulator),
            0x6c => address!(jmp, indirect),
            0x6d => read!(adc, absolute),
            0x6e => modify!(ror, absolute),
            0x70 => branch!(overflow, true),
            0x71 => read!(adc, indirect_indexed_cross),
            0x75 => read!(adc, zeropage_x),
            0x76 => modify!(ror, zeropage_x),
            0x78 => set!(interrupt_disable, true),
            0x79 => read!(adc, absolute_y_cross),
            0x7d => read!(adc, absolute_x_cross),
            0x7e => modify!(ror, absolute_x),
            0x81 => store!(accumulator, indexed_indirect),
            0x84 => store!(y, zeropage),
            0x85 => store!(accumulator, zeropage),
            0x86 => store!(x, zeropage),
            0x88 => self.dey(),
            0x8a => transfer!(x, accumulator),
            0x8c => store!(y, absolute),
            0x8d => store!(accumulator, absolute),
            0x8e => store!(x, absolute),
            0x90 => branch!(carry, false),
            0x91 => store!(accumulator, indirect_indexed),
            0x94 => store!(y, zeropage_x),
            0x95 => store!(accumulator, zeropage_x),
            0x96 => store!(x, zeropage_y),
            0x98 => transfer!(y, accumulator),
            0x99 => store!(accumulator, absolute_y),
            0x9a => transfer!(x, stack_pointer),
            0x9d => store!(accumulator, absolute_x),
            0xa0 => read!(ldy, immediate),
            0xa1 => read!(lda, indexed_indirect),
            0xa2 => read!(ldx, immediate),
            0xa4 => read!(ldy, zeropage),
            0xa5 => read!(lda, zeropage),
            0xa6 => read!(ldx, zeropage),
            0xa8 => transfer!(accumulator, y),
            0xa9 => read!(lda, immediate),
            0xaa => transfer!(accumulator, x),
            0xac => read!(ldy, absolute),
            0xad => read!(lda, absolute),
            0xae => read!(ldx, absolute),
            0xb0 => branch!(carry, true),
            0xb1 => read!(lda, indirect_indexed_cross),
            0xb4 => read!(ldy, zeropage_x),
            0xb5 => read!(lda, zeropage_x),
            0xb6 => read!(ldx, zeropage_y),
            0xb8 => set!(overflow, false),
            0xb9 => read!(lda, absolute_y_cross),
            0xba => transfer!(stack_pointer, x),
            0xbc => read!(ldy, absolute_x_cross),
            0xbd => read!(lda, absolute_x_cross),
            0xbe => read!(ldx, absolute_y_cross),
            0xc0 => read!(cpy, immediate),
            0xc1 => read!(cmp, indexed_indirect),
            0xc4 => read!(cpy, zeropage),
            0xc5 => read!(cmp, zeropage),
            0xc6 => modify!(dec, zeropage),
            0xc8 => self.iny(),
            0xc9 => read!(cmp, immediate),
            0xca => self.dex(),
            0xcc => read!(cpy, absolute),
            0xcd => read!(cmp, absolute),
            0xce => modify!(dec, absolute),
            0xd0 => branch!(zero, false),
            0xd1 => read!(cmp, indirect_indexed_cross),
            0xd5 => read!(cmp, zeropage_x),
            0xd6 => modify!(dec, zeropage_x),
            0xd8 => set!(decimal_mode, false),
            0xd9 => read!(cmp, absolute_y_cross),
            0xdd => read!(cmp, absolute_x_cross),
            0xde => modify!(dec, absolute_x),
            0xe0 => read!(cpx, immediate),
            0xe1 => read!(sbc, indexed_indirect),
            0xe4 => read!(cpx, zeropage),
            0xe5 => read!(sbc, zeropage),
            0xe6 => modify!(inc, zeropage),
            0xe8 => self.inx(),
            0xe9 => read!(sbc, immediate),
            0xea => self.nop(),
            0xec => read!(cpx, absolute),
            0xed => read!(sbc, absolute),
            0xee => modify!(inc, absolute),
            0xf0 => branch!(zero, true),
            0xf1 => read!(sbc, indirect_indexed_cross),
            0xf5 => read!(sbc, zeropage_x),
            0xf6 => modify!(inc, zeropage_x),
            0xf8 => set!(decimal_mode, true),
            0xf9 => read!(sbc, absolute_y_cross),
            0xfd => read!(sbc, absolute_x_cross),
            0xfe => modify!(inc, absolute_x),
            _ => unimplemented!("Encountered unimplemented opcode ${:x}", opcode),
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
                _ => yields().await,
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
        let old_page = self.program_counter.high_byte();
        let sign = offset.bit(7);
        let offset = offset.bits(0, 7) as u16;

        self.program_counter = if sign {
            self.program_counter.wrapping_sub(offset)
        } else {
            self.program_counter.wrapping_add(offset)
        };

        let new_page = self.program_counter.high_byte();

        if old_page != new_page {
            self.clock.advance(1);
        }
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
        if address.high_byte() != effective_address.high_byte() {
            self.clock.advance(1);
        }
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
        if address.high_byte() != effective_address.high_byte() {
            self.clock.advance(1);
        }
        effective_address
    }

    /**
     * Zeropage addressing operates on the address obtained by interpreting the
     * immediate operand as an address.
     */
    async fn zeropage(&mut self, bus: &impl Bus) -> u16{
        self.fetch(bus).await as u16
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by adding
     * an index register to the immediate operand, and interpreting that as an
     * address.
     * Note: the high byte is always zero, page boundary crossings are not
     * possible.
     */
    async fn zeropage_x(&mut self, bus: &impl Bus) -> u16 {
        let address = self.fetch(bus).await.wrapping_add(self.x) as u16;
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
    async fn zeropage_y(&mut self, bus: &impl Bus) -> u16 {
        let address = self.fetch(bus).await.wrapping_add(self.y) as u16;
        self.clock.advance(1);
        address
    }

    /**
     * Indirect addressing uses a 16-bit operand as address from which it reads
     * the actual address to operate on.
     */
    async fn indirect(&mut self, bus: &impl Bus) -> u16 {
        let address = self.absolute(bus).await;
        let low_byte = self.read(bus, address).await;
        let high_byte = self.read(bus, address + 1).await;
        Word::from_bytes(low_byte, high_byte)
    }

    /**
     * Indexed indirect reads the 16-bit target address from the memory found
     * at a zero page X-indexed address.
     */
    async fn indexed_indirect(&mut self, bus: &impl Bus) -> u16 {
        let address = self.zeropage_x(bus).await;
        let low_byte = self.read(bus, address).await;
        let high_byte = self.read(bus, address.wrapping_add(1)).await;
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
        let low_byte = self.read(bus, zeropage_address).await;
        let high_byte = self.read(bus, zeropage_address.wrapping_add(1)).await;
        let address = u16::from_bytes(low_byte, high_byte);
        let effective_address = address.wrapping_add(self.y as u16);
        self.clock.advance(1);
        effective_address
    }

    async fn indirect_indexed_cross(&mut self, bus: &impl Bus) -> u16 {
        let zeropage_address = self.zeropage(bus).await;
        let low_byte = self.read(bus, zeropage_address).await;
        let high_byte = self.read(bus, zeropage_address.wrapping_add(1)).await;
        let address = u16::from_bytes(low_byte, high_byte);
        let effective_address = address.wrapping_add(self.y as u16);
        if address.high_byte() != effective_address.high_byte() {
            self.clock.advance(1);
        }
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
        self.status.zero = result == 0;
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
        self.status.overflow = result.bit(6);
        self.status.negative = result.bit(7);
    }

    /**
     * Compare accumulator with memory
     */
    fn cmp(&mut self, operand: u8) {
        let result = self.accumulator as i16 - operand as i16;
        self.status.zero = result == 0;
        self.status.carry = result >= 0;
        self.status.negative = (result as u8).bit(7);
    }

    /**
     * Compare X register with memory
     */
    fn cpx(&mut self, operand: u8) {
        let result = self.x as i16 - operand as i16;
        self.status.zero = result == 0;
        self.status.carry = result >= 0;
        self.status.negative = (result as u8).bit(7);
    }

    /**
     * Compare Y register with memory
     */
    fn cpy(&mut self, operand: u8) {
        let result = self.y as i16 - operand as i16;
        self.status.zero = result == 0;
        self.status.carry = result >= 0;
        self.status.negative = (result as u8).bit(7);
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
        self.clock.advance(1);
        self.x = self.x.wrapping_sub(1);
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Decrement Y register
     */
    fn dey(&mut self) {
        self.clock.advance(1);
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
        self.clock.advance(1);
        self.x = self.x.wrapping_add(1);
        self.status.zero = self.x == 0;
        self.status.negative = self.x.bit(7);
    }

    /**
     * Increment Y register
     */
    fn iny(&mut self) {
        self.clock.advance(1);
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
    fn nop(&mut self) {
        self.clock.advance(1);
    }

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
        let mut result = operand << 1;
        result.change_bit(0, self.status.carry);
        self.status.carry = operand.bit(7);
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        result
    }

    /**
     * Rotate right
     */
    fn ror(&mut self, operand: u8) -> u8 {
        let mut result = operand >> 1;
        result.change_bit(7, self.status.carry);
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
        self.program_counter = Word::from_bytes(
            self.pull(bus).await,
            self.pull(bus).await
        );
        self.program_counter = self.program_counter.wrapping_add(1);
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
        Ok(())
    }
}


#[cfg(test)]
mod test {
    use super::*;

    mod cpu {
        use super::*;
        use std::cell::Cell;

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
        }

        impl cpu::Bus for ArrayBus {
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
        fn instruction_characteristics() {
            let mut cpu = Ricoh2A03::new();
            let bus = ArrayBus::new();

            /**
             * We check for the timing and bytes by simply running the
             * instruction in a sandbox environment.
             */
            macro_rules! check_instruction {
                ($instruction:expr, $addressing:expr, $opcode:expr, $cycles:expr, $bytes:expr) => {{
                    check_cycles!($instruction, $addressing, $opcode, $cycles);
                    check_bytes!($instruction, $addressing, $opcode, $bytes);
                }}
            }

            macro_rules! check_cycles {
                ($instruction:expr, $addressing:expr, $opcode:expr, $cycles:expr) => {{
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
            }

            macro_rules! check_bytes {
                ($instruction:expr, $addressing:expr, $opcode:expr, $bytes:expr) => {{
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
            }


            /**
             * We check branch timing for three scenario's:
             * - Branch not taken: nominal amount of cycles
             * - Branch taken: nominal cycles plus one
             * - Branch taken to different page: nominal cycles plus two
             */
            macro_rules! check_branch_timing {
                ($instruction:expr, $addressing:expr, $opcode:expr, $cycles:expr, $flag:ident, $value:expr) => {{
                    cpu.status.$flag = !($value);
                    check_instruction!($instruction, $addressing, $opcode, $cycles, 2);

                    cpu.status.$flag = $value;
                    bus.data[0xf0].set(0x00);
                    check_cycles!($instruction, $addressing, $opcode, $cycles + 1);

                    bus.data[0xf0].set(0x0f);
                    check_cycles!($instruction, $addressing, $opcode, $cycles + 2);
                }}
            }

            // Timings taken from: obelisk.me.uk/6502/reference.html
            check_instruction!("ADC", "Immediate", 0x69, 2, 2);
            check_instruction!("ADC", "Zeropage", 0x65, 3, 2);
            check_instruction!("ADC", "Zeropage,X", 0x75, 4, 2);
            check_instruction!("ADC", "Absolute", 0x6d, 4, 3);
            check_instruction!("ADC", "Absolute,X", 0x7d, 4, 3);
            check_instruction!("ADC", "Absolute,Y", 0x79, 4, 3);
            check_instruction!("ADC", "(Indirect,X)", 0x61, 6, 2);
            check_instruction!("ADC", "(Indirect),Y", 0x71, 5, 2);

            check_instruction!("AND", "Immediate", 0x29, 2, 2);
            check_instruction!("AND", "Zeropage", 0x25, 3, 2);
            check_instruction!("AND", "Zeropage,X", 0x35, 4, 2);
            check_instruction!("AND", "Absolute", 0x2d, 4, 3);
            check_instruction!("AND", "Absolute,X", 0x3d, 4, 3);
            check_instruction!("AND", "Absolute,Y", 0x39, 4, 3);
            check_instruction!("AND", "(Indirect,X)", 0x21, 6, 2);
            check_instruction!("AND", "(Indirect),Y", 0x31, 5, 2);

            check_instruction!("ASL", "Accumulator", 0x0a, 2, 1);
            check_instruction!("ASL", "Zeropage", 0x06, 5, 2);
            check_instruction!("ASL", "Zeropage,X", 0x16, 6, 2);
            check_instruction!("ASL", "Absolute", 0x0e, 6, 3);
            check_instruction!("ASL", "Absolute,X", 0x1e, 7, 3);

            check_instruction!("BIT", "Zeropage", 0x24, 3, 2);
            check_instruction!("BIT", "Absolute", 0x2c, 4, 3);

            check_instruction!("CLC", "Implied", 0x18, 2, 1);
            check_instruction!("CLD", "Implied", 0xd8, 2, 1);
            check_instruction!("CLI", "Implied", 0x58, 2, 1);
            check_instruction!("CLV", "Implied", 0xb8, 2, 1);

            check_instruction!("SEC", "Implied", 0x38, 2, 1);
            check_instruction!("SED", "Implied", 0xf8, 2, 1);
            check_instruction!("SEI", "Implied", 0x78, 2, 1);

            check_instruction!("CMP", "Immediate", 0xc9, 2, 2);
            check_instruction!("CMP", "Zeropage", 0xc5, 3, 2);
            check_instruction!("CMP", "Zeropage,X", 0xd5, 4, 2);
            check_instruction!("CMP", "Absolute", 0xcd, 4, 3);
            check_instruction!("CMP", "Absolute,X", 0xdd, 4, 3);
            check_instruction!("CMP", "Absolute,Y", 0xd9, 4, 3);
            check_instruction!("CMP", "(Indirect,X)", 0xc1, 6, 2);
            check_instruction!("CMP", "(Indirect),Y", 0xd1, 5, 2);

            check_instruction!("CPX", "Immediate", 0xe0, 2, 2);
            check_instruction!("CPX", "Zeropage", 0xe4, 3, 2);
            check_instruction!("CPX", "Absolute", 0xec, 4, 3);

            check_instruction!("CPY", "Immediate", 0xc0, 2, 2);
            check_instruction!("CPY", "Zeropage", 0xc4, 3, 2);
            check_instruction!("CPY", "Absolute", 0xcc, 4, 3);

            check_instruction!("DEC", "Zeropage", 0xc6, 5, 2);
            check_instruction!("DEC", "Zeropage,X", 0xd6, 6, 2);
            check_instruction!("DEC", "Absolute", 0xce, 6, 3);
            check_instruction!("DEC", "Absolute,X", 0xde, 7, 3);

            check_instruction!("DEX", "Implied", 0xca, 2, 1);
            check_instruction!("DEY", "Implied", 0x88, 2, 1);

            check_instruction!("EOR", "Immediate", 0x49, 2, 2);
            check_instruction!("EOR", "Zeropage", 0x45, 3, 2);
            check_instruction!("EOR", "Zeropage,X", 0x55, 4, 2);
            check_instruction!("EOR", "Absolute", 0x4d, 4, 3);
            check_instruction!("EOR", "Absolute,X", 0x5d, 4, 3);
            check_instruction!("EOR", "Absolute,Y", 0x59, 4, 3);
            check_instruction!("EOR", "(Indirect,X)", 0x41, 6, 2);
            check_instruction!("EOR", "(Indirect),Y", 0x51, 5, 2);

            check_instruction!("INC", "Zeropage", 0xe6, 5, 2);
            check_instruction!("INC", "Zeropage,X", 0xf6, 6, 2);
            check_instruction!("INC", "Absolute", 0xee, 6, 3);
            check_instruction!("INC", "Absolute,X", 0xfe, 7, 3);

            check_instruction!("INX", "Implied", 0xe8, 2, 1);
            check_instruction!("INY", "Implied", 0xc8, 2, 1);

            check_instruction!("ORA", "Immediate", 0x09, 2, 2);
            check_instruction!("ORA", "Zeropage", 0x05, 3, 2);
            check_instruction!("ORA", "Zeropage,X", 0x15, 4, 2);
            check_instruction!("ORA", "Absolute", 0x0d, 4, 3);
            check_instruction!("ORA", "Absolute,X", 0x1d, 4, 3);
            check_instruction!("ORA", "Absolute,Y", 0x19, 4, 3);
            check_instruction!("ORA", "(Indirect,X)", 0x01, 6, 2);
            check_instruction!("ORA", "(Indirect),Y", 0x11, 5, 2);

            check_instruction!("LDA", "Immediate", 0xa9, 2, 2);
            check_instruction!("LDA", "Zeropage", 0xa5, 3, 2);
            check_instruction!("LDA", "Zeropage,X", 0xb5, 4, 2);
            check_instruction!("LDA", "Absolute", 0xad, 4, 3);
            check_instruction!("LDA", "Absolute,X", 0xbd, 4, 3);
            check_instruction!("LDA", "Absolute,Y", 0xb9, 4, 3);
            check_instruction!("LDA", "(Indirect,X)", 0xa1, 6, 2);
            check_instruction!("LDA", "(Indirect),Y", 0xb1, 5, 2);

            check_instruction!("LDX", "Immediate", 0xa2, 2, 2);
            check_instruction!("LDX", "Zeropage", 0xa6, 3, 2);
            check_instruction!("LDX", "Zeropage,Y", 0xb6, 4, 2);
            check_instruction!("LDX", "Absolute", 0xae, 4, 3);
            check_instruction!("LDX", "Absolute,Y", 0xbe, 4, 3);
            
            check_instruction!("LDY", "Immediate", 0xa0, 2, 2);
            check_instruction!("LDY", "Zeropage", 0xa4, 3, 2);
            check_instruction!("LDY", "Zeropage,X", 0xb4, 4, 2);
            check_instruction!("LDY", "Absolute", 0xac, 4, 3);
            check_instruction!("LDY", "Absolute,X", 0xbc, 4, 3);

            check_instruction!("LSR", "Accumulator", 0x4a, 2, 1);
            check_instruction!("LSR", "Zeropage", 0x46, 5, 2);
            check_instruction!("LSR", "Zeropage,X", 0x56, 6, 2);
            check_instruction!("LSR", "Absolute", 0x4e, 6, 3);
            check_instruction!("LSR", "Absolute,X", 0x5e, 7, 3);

            check_instruction!("NOP", "Implied", 0xea, 2, 1);

            check_instruction!("PHA", "Implied", 0x48, 3, 1);
            check_instruction!("PHP", "Implied", 0x08, 3, 1);
            check_instruction!("PLA", "Implied", 0x68, 4, 1);
            check_instruction!("PLP", "Implied", 0x28, 4, 1);

            check_instruction!("ROL", "Accumulator", 0x2a, 2, 1);
            check_instruction!("ROL", "Zeropage", 0x26, 5, 2);
            check_instruction!("ROL", "Zeropage,X", 0x36, 6, 2);
            check_instruction!("ROL", "Absolute", 0x2e, 6, 3);
            check_instruction!("ROL", "Absolute,X", 0x3e, 7, 3);

            check_instruction!("ROR", "Accumulator", 0x6a, 2, 1);
            check_instruction!("ROR", "Zeropage", 0x66, 5, 2);
            check_instruction!("ROR", "Zeropage,X", 0x76, 6, 2);
            check_instruction!("ROR", "Absolute", 0x6e, 6, 3);
            check_instruction!("ROR", "Absolute,X", 0x7e, 7, 3);

            check_instruction!("SBC", "Immediate", 0xe9, 2, 2);
            check_instruction!("SBC", "Zeropage", 0xe5, 3, 2);
            check_instruction!("SBC", "Zeropage,X", 0xf5, 4, 2);
            check_instruction!("SBC", "Absolute", 0xed, 4, 3);
            check_instruction!("SBC", "Absolute,X", 0xfd, 4, 3);
            check_instruction!("SBC", "Absolute,Y", 0xf9, 4, 3);
            check_instruction!("SBC", "(Indirect,X)", 0xe1, 6, 2);
            check_instruction!("SBC", "(Indirect),Y", 0xf1, 5, 2);

            check_instruction!("STA", "Zeropage", 0x85, 3, 2);
            check_instruction!("STA", "Zeropage,X", 0x95, 4, 2);
            check_instruction!("STA", "Absolute", 0x8d, 4, 3);
            check_instruction!("STA", "Absolute,X", 0x9d, 5, 3);
            check_instruction!("STA", "Absolute,Y", 0x99, 5, 3);
            check_instruction!("STA", "(Indirect,X)", 0x81, 6, 2);
            check_instruction!("STA", "(Indirect),Y", 0x91, 6, 2);

            check_instruction!("STX", "Zeropage", 0x86, 3, 2);
            check_instruction!("STX", "Zeropage,X", 0x96, 4, 2);
            check_instruction!("STX", "Absolute", 0x8e, 4, 3);
            check_instruction!("STY", "Zeropage", 0x84, 3, 2);
            check_instruction!("STY", "Zeropage,X", 0x94, 4, 2);
            check_instruction!("STY", "Absolute", 0x8c, 4, 3);

            check_instruction!("TAX", "Implied", 0xaa, 2, 1);
            check_instruction!("TAY", "Implied", 0xa8, 2, 1);
            check_instruction!("TSX", "Implied", 0xba, 2, 1);
            check_instruction!("TXA", "Implied", 0x8a, 2, 1);
            check_instruction!("TXS", "Implied", 0x9a, 2, 1);
            check_instruction!("TYA", "Implied", 0x98, 2, 1);
            
            // Number of bytes "used" by a jump instruction is irrelevant, as
            // the program counter is shifted anyway.
            check_cycles!("BRK", "Implied", 0x00, 7);
            check_cycles!("JMP", "Absolute", 0x4c, 3);
            check_cycles!("JMP", "Indirect", 0x6c, 5);
            check_cycles!("JSR", "Absolute", 0x20, 6);
            check_cycles!("RTI", "Implied", 0x40, 6);
            check_cycles!("RTS", "Implied", 0x60, 6);

            // Branch instructions take a variable number of cycles, so must be
            // checked for each of those scenarios.
            check_branch_timing!("BCS", "Relative", 0xb0, 2, carry, true);
            check_branch_timing!("BCC", "Relative", 0x90, 2, carry, false);
            check_branch_timing!("BEQ", "Relative", 0xf0, 2, zero, true);
            check_branch_timing!("BNE", "Relative", 0xd0, 2, zero, false);
            check_branch_timing!("BVS", "Relative", 0x70, 2, overflow, true);
            check_branch_timing!("BVC", "Relative", 0x50, 2, overflow, false);
            check_branch_timing!("BMI", "Relative", 0x30, 2, negative, true);
            check_branch_timing!("BPL", "Relative", 0x10, 2, negative, false);
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
            assert_eq!(format!("{:?}", status), "nv--dIzc");
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
            assert_eq!(format!("{:?}", status), "NV--DIZC");

            status.carry = false;
            status.zero = false;
            status.interrupt_disable = false;
            status.decimal_mode = false;
            status.overflow = false;
            status.negative = false;
            assert_eq!(format!("{:?}", status), "nv--dizc");
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
            assert_eq!(format!("{:?}", status), "NV--DIZC");

            let status: Status = 0b00000000.into();
            assert_eq!(status.interrupt_value(), 0b00100000);
            assert_eq!(status.instruction_value(), 0b00110000);
            assert_eq!(status.carry, false);
            assert_eq!(status.zero, false);
            assert_eq!(status.interrupt_disable, false);
            assert_eq!(status.decimal_mode, false);
            assert_eq!(status.overflow, false);
            assert_eq!(status.negative, false);
            assert_eq!(format!("{:?}", status), "nv--dizc");
        }        
    }
}