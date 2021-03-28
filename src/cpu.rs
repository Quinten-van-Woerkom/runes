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
pub struct Ricoh2A03 {
    clock: Clock,
    program_counter: u16,
    status: Status,
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
            ($flag:ident) => {{
                let offset = self.fetch(bus).await;
                if self.status.$flag {
                    self.branch(offset);
                }
            }};

            (!$flag:ident) => {{
                let offset = self.fetch(bus).await;
                if !self.status.$flag {
                    self.branch(offset);
                }
            }};
        }

        /**
         * Clears a given status flag.
         */
        macro_rules! clear {
            ($flag:ident) => {{
                self.clock.advance(1);
                self.status.$flag = false;
            }}
        }

        /**
         * Sets a given status flag.
         */
        macro_rules! set {
            ($flag:ident) => {{
                self.clock.advance(1);
                self.status.$flag = true;
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
                let (address, crossed) = self.$addressing(bus).await;
                if crossed {
                    self.clock.advance(1);
                }
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

            ($instruction:ident, absolute_x) => {{
                let (address, _) = self.absolute_x(bus).await;
                self.clock.advance(1);
                let operand = self.read(bus, address).await;
                let result = self.$instruction(operand);
                self.clock.advance(1);
                self.write(bus, address, result).await;
            }};

            ($instruction:ident, $addressing:ident) => {{
                let (address, _) = self.$addressing(bus).await;
                let operand = self.read(bus, address).await;
                let result = self.$instruction(operand);
                self.clock.advance(1);
                self.write(bus, address, result).await;
            }};
        }

        match opcode {
            0x06 => modify!(asl, zeropage),
            0x0a => modify!(asl, accumulator),
            0x0e => modify!(asl, absolute),
            0x10 => branch!(!negative),
            0x16 => modify!(asl, zeropage_x),
            0x18 => clear!(carry),
            0x1e => modify!(asl, absolute_x),
            0x21 => read!(and, indexed_indirect),
            0x24 => read!(bit, zeropage),
            0x25 => read!(and, zeropage),
            0x29 => read!(and, immediate),
            0x2c => read!(bit, absolute),
            0x2d => read!(and, absolute),
            0x30 => branch!(negative),
            0x31 => read!(and, indirect_indexed),
            0x35 => read!(and, zeropage_x),
            0x38 => set!(carry),
            0x39 => read!(and, absolute_y),
            0x3d => read!(and, absolute_x),
            0x50 => branch!(!overflow),
            0x58 => clear!(interrupt_disable),
            0x61 => read!(adc, indexed_indirect),
            0x65 => read!(adc, zeropage),
            0x69 => read!(adc, immediate),
            0x6d => read!(adc, absolute),
            0x70 => branch!(overflow),
            0x71 => read!(adc, indirect_indexed),
            0x75 => read!(adc, zeropage_x),
            0x78 => set!(interrupt_disable),
            0x7d => read!(adc, absolute_x),
            0x79 => read!(adc, absolute_y),
            0x90 => branch!(!carry),
            0xb0 => branch!(carry),
            0xb8 => clear!(overflow),
            0xc0 => read!(cpy, immediate),
            0xc1 => read!(cmp, indexed_indirect),
            0xc4 => read!(cpy, zeropage),
            0xc5 => read!(cmp, zeropage),
            0xc6 => modify!(dec, zeropage),
            0xc9 => read!(cmp, immediate),
            0xcc => read!(cpy, absolute),
            0xcd => read!(cmp, absolute),
            0xce => modify!(dec, absolute),
            0xd0 => branch!(!zero),
            0xd1 => read!(cmp, indirect_indexed),
            0xd5 => read!(cmp, zeropage_x),
            0xd6 => modify!(dec, zeropage_x),
            0xd8 => clear!(decimal_mode),
            0xd9 => read!(cmp, absolute_y),
            0xdd => read!(cmp, absolute_x),
            0xde => modify!(dec, absolute_x),
            0xe0 => read!(cpx, immediate),
            0xe4 => read!(cpx, zeropage),
            0xec => read!(cpx, absolute),
            0xf0 => branch!(zero),
            0xf8 => set!(decimal_mode),
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
        self.program_counter += 1;
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
    async fn immediate(&mut self, _bus: &impl Bus) -> (u16, bool) {
        let address = self.program_counter;
        self.program_counter = self.program_counter.wrapping_add(1);
        (address, false)
    }

    /**
     * Absolute addressing loads the two bytes after the opcode into a 16-bit
     * word that it uses as effective address.
     */
    async fn absolute(&mut self, bus: &impl Bus) -> (u16, bool) {
        let low_byte = self.fetch(bus).await;
        let high_byte = self.fetch(bus).await;
        (Word::from_bytes(low_byte, high_byte), false)
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     */
    async fn absolute_x(&mut self, bus: &impl Bus) -> (u16, bool) {
        let (address, _) = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.x as u16);
        let page_crossed = address.high_byte() != effective_address.high_byte();
        (effective_address, page_crossed)
    }

    /**
     * Indexed absolute addressing adds one of the index registers to the
     * address obtained from absolute addressing. Write instructions or read
     * instructions crossing a page boundary take one extra cycle.
     */
    async fn absolute_y(&mut self, bus: &impl Bus) -> (u16, bool) {
        let (address, _) = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.y as u16);
        let page_crossed = address.high_byte() != effective_address.high_byte();
        (effective_address, page_crossed)
    }

    /**
     * Zeropage addressing operates on the address obtained by interpreting the
     * immediate operand as an address.
     */
    async fn zeropage(&mut self, bus: &impl Bus) -> (u16, bool) {
        (self.fetch(bus).await as u16, false)
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by adding
     * an index register to the immediate operand, and interpreting that as an
     * address.
     * Note: the high byte is always zero, page boundary crossings are not
     * possible.
     */
    async fn zeropage_x(&mut self, bus: &impl Bus) -> (u16, bool) {
        let address = self.fetch(bus).await.wrapping_add(self.x) as u16;
        self.clock.advance(1);
        (address, false)
    }

    /**
     * Zeropage indexed addressing operates on the address obtained by adding
     * an index register to the immediate operand, and interpreting that as an
     * address.
     * Note: the high byte is always zero, page boundary crossings are not
     * possible.
     */
    async fn zeropage_y(&mut self, bus: &impl Bus) -> (u16, bool) {
        let address = self.fetch(bus).await.wrapping_add(self.y) as u16;
        self.clock.advance(1);
        (address, false)
    }

    /**
     * Indirect addressing uses a 16-bit operand as address from which it reads
     * the actual address to operate on.
     */
    async fn indirect(&mut self, bus: &impl Bus) -> (u16, bool) {
        let (address, _) = self.absolute(bus).await;
        let low_byte = self.read(bus, address).await;
        let high_byte = self.read(bus, address + 1).await;
        (Word::from_bytes(low_byte, high_byte), false)
    }

    /**
     * Indexed indirect reads the 16-bit target address from the memory found
     * at a zero page X-indexed address.
     */
    async fn indexed_indirect(&mut self, bus: &impl Bus) -> (u16, bool) {
        let (address, _) = self.zeropage_x(bus).await;
        let low_byte = self.read(bus, address).await;
        let high_byte = self.read(bus, address.wrapping_add(1)).await;
        (Word::from_bytes(low_byte, high_byte), false)
    }

    /**
     * Indirect indexed adds the Y register to the 16-bit address read from the
     * location found using zero page indexing.
     * If a page boundary is crossed when adding Y, an extra cycle might be
     * used, depending on the instruction.
     */
    async fn indirect_indexed(&mut self, bus: &impl Bus) -> (u16, bool) {
        let (zeropage_address, _) = self.zeropage(bus).await;
        let low_byte = self.read(bus, zeropage_address).await;
        let high_byte = self.read(bus, zeropage_address.wrapping_add(1)).await;
        let address = u16::from_bytes(low_byte, high_byte);
        let effective_address = address.wrapping_add(self.y as u16);
        let page_crossed = address.high_byte() != effective_address.high_byte();
        (effective_address, page_crossed)
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
     * This refers to the non-accumulator version. The accumulator addressing
     * variant is implemented as if it were a separate instruction, as it
     * practically is.
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
        fn instruction_timing() {
            let mut cpu = Ricoh2A03::new();
            let bus = ArrayBus::new();

            /**
             * We check for the timing by simply running the instruction in a
             * sandbox environment.
             */
            macro_rules! check_timing {
                ($instruction:expr, $addressing:expr, $opcode:expr, $cycles:expr, $bytes:expr) => {{
                    let start = cpu.clock.current();
                    let first_byte = cpu.program_counter;
                    futures::executor::block_on(cpu.execute(&bus, $opcode));

                    // Add one cycle to compensate for missing opcode read
                    let cycles = cpu.clock.current() - start + 1;
                    assert_eq!(
                        cycles, $cycles,
                        "Incorrect number of cycles for {} {}: \
                        is {}, should be {}, PC={}",
                        $instruction, $addressing, cycles, $cycles, cpu.program_counter
                    );

                    // Add one byte to compensate for missing opcode read
                    let bytes = cpu.program_counter - first_byte + 1;
                    assert_eq!(
                        bytes, $bytes,
                        "Incorrect number of bytes for {} {}: \
                        is {}, should be {}, PC={}",
                        $instruction, $addressing, bytes, $bytes, cpu.program_counter
                    );
                }};

                ($instruction:expr, $addressing:expr, $opcode:expr, $cycles:expr) => {{
                    let start = cpu.clock.current();
                    let first_byte = cpu.program_counter;
                    futures::executor::block_on(cpu.execute(&bus, $opcode));

                    // Add one cycle to compensate for opcode read
                    let cycles = cpu.clock.current() - start + 1;

                    assert_eq!(
                        cycles, $cycles,
                        "Incorrect number of cycles for {} {}: \
                        is {}, should be {}, PC={}",
                        $instruction, $addressing, cycles, $cycles, cpu.program_counter
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
                    cpu.program_counter = 0;

                    cpu.status.$flag = !($value);
                    check_timing!($instruction, $addressing, $opcode, $cycles);


                    cpu.status.$flag = $value;
                    check_timing!($instruction, $addressing, $opcode, $cycles + 1);

                    cpu.program_counter = 0xf1;
                    bus.data[0xf1].set(0x1f);
                    check_timing!($instruction, $addressing, $opcode, $cycles + 2);
                }}
            }

            // Timings taken from: obelisk.me.uk/6502/reference.html
            check_timing!("ADC", "Immediate", 0x69, 2, 2);
            check_timing!("ADC", "Zeropage", 0x65, 3, 2);
            check_timing!("ADC", "Zeropage,X", 0x75, 4, 2);
            check_timing!("ADC", "Absolute", 0x6d, 4, 3);
            check_timing!("ADC", "Absolute,X", 0x7d, 4, 3);
            check_timing!("ADC", "Absolute,Y", 0x79, 4, 3);
            check_timing!("ADC", "(Indirect,X)", 0x61, 6, 2);
            check_timing!("ADC", "(Indirect),Y", 0x71, 5, 2);

            check_timing!("AND", "Immediate", 0x29, 2);
            check_timing!("AND", "Zeropage", 0x25, 3);
            check_timing!("AND", "Zeropage,X", 0x35, 4);
            check_timing!("AND", "Absolute", 0x2d, 4);
            check_timing!("AND", "Absolute,X", 0x3d, 4);
            check_timing!("AND", "Absolute,Y", 0x39, 4);
            check_timing!("AND", "(Indirect,X)", 0x21, 6);
            check_timing!("AND", "(Indirect),Y", 0x31, 5);

            check_timing!("ASL", "Zeropage", 0x06, 5, 2);
            check_timing!("ASL", "Zeropage,X", 0x16, 6, 2);
            check_timing!("ASL", "Absolute", 0x0e, 6, 3);
            check_timing!("ASL", "Absolute,X", 0x1e, 7, 3);

            check_timing!("BIT", "Zeropage", 0x24, 3, 2);
            check_timing!("BIT", "Absolute", 0x2c, 4, 3);

            check_timing!("CLC", "Implied", 0x18, 2, 1);
            check_timing!("CLD", "Implied", 0xd8, 2, 1);
            check_timing!("CLI", "Implied", 0x58, 2, 1);
            check_timing!("CLV", "Implied", 0xb8, 2, 1);

            check_timing!("SEC", "Implied", 0x38, 2, 1);
            check_timing!("SED", "Implied", 0xf8, 2, 1);
            check_timing!("SEI", "Implied", 0x78, 2, 1);

            check_timing!("CMP", "Immediate", 0xc9, 2, 2);
            check_timing!("CMP", "Zeropage", 0xc5, 3, 2);
            check_timing!("CMP", "Zeropage,X", 0xd5, 4, 2);
            check_timing!("CMP", "Absolute", 0xcd, 4, 3);
            check_timing!("CMP", "Absolute,X", 0xdd, 4, 3);
            check_timing!("CMP", "Absolute,Y", 0xd9, 4, 3);
            check_timing!("CMP", "(Indirect,X)", 0xc1, 6, 2);
            check_timing!("CMP", "(Indirect),Y", 0xd1, 5, 2);

            check_timing!("CPX", "Immediate", 0xe0, 2, 2);
            check_timing!("CPX", "Zeropage", 0xe4, 3, 2);
            check_timing!("CPX", "Absolute", 0xec, 4, 3);

            check_timing!("CPY", "Immediate", 0xc0, 2, 2);
            check_timing!("CPY", "Zeropage", 0xc4, 3, 2);
            check_timing!("CPY", "Absolute", 0xcc, 4, 3);

            check_timing!("DEC", "Zeropage", 0xc6, 5, 2);
            check_timing!("DEC", "Zeropage,X", 0xd6, 6, 2);
            check_timing!("DEC", "Absolute", 0xce, 6, 3);
            check_timing!("DEC", "Absolute,X", 0xde, 7, 3);

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

            // Accumulator instructions are implemented separately.
            check_timing!("ASL", "Accumulator", 0x0a, 2, 1);
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