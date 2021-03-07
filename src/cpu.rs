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

    async fn execute(&mut self, bus: &impl Bus, opcode: u8) {
        /**
         * The following shorthand is used to compose addressing modes and
         * instructions.
         */
        macro_rules! instruction {
            ($operation:ident, implied) => {{
                self.$operation()
            }};

            ($conditional_branch:ident, relative, $flag:ident, $value:expr) => {{
                let offset = self.fetch(bus).await;
                if self.status.$flag == $value {
                    self.branch(offset);
                }
            }};

            ($operation:ident, $addressing:ident) => {{
                let address = self.$addressing(bus).await;
                self.$operation(bus, address).await;
            }};


            // Absolute,X addressing take one less cycle if a read
            // instruction does not cross page boundaries while addressing.
            ($operation:ident, absolute_x, $read_instruction:expr) => {{
                let (address, page_crossing) = self.absolute_x(bus).await;
                if !$read_instruction || page_crossing {
                    self.clock.advance(1);
                }
                self.$operation(bus, address).await;
            }};

            // Indirect indexed addressing take one less cycle if a read
            // instruction does not cross page boundaries while addressing.
            ($operation:ident, indirect_indexed, $read_instruction:expr) => {{
                let (address, page_crossing) = self.indirect_indexed(bus).await;
                if !$read_instruction || page_crossing {
                    self.clock.advance(1);
                }
                self.$operation(bus, address).await;
            }};
        }

        match opcode {
            0x06 => instruction!(asl, zeropage),
            0x0a => instruction!(asl_accumulator, implied),
            0x0e => instruction!(asl, absolute),
            0x10 => instruction!(bpl, relative, negative, false),
            0x16 => instruction!(asl, zeropage_x),
            0x1e => instruction!(asl, absolute_x, false),
            0x21 => instruction!(and, indexed_indirect),
            0x25 => instruction!(and, zeropage),
            0x29 => instruction!(and, immediate),
            0x2d => instruction!(and, absolute),
            0x30 => instruction!(bmi, relative, negative, true),
            0x31 => instruction!(and, indirect_indexed, true),
            0x35 => instruction!(and, zeropage_x),
            0x39 => instruction!(and, absolute_y),
            0x3d => instruction!(and, absolute_x, true),
            0x50 => instruction!(bvc, relative, overflow, false),
            0x61 => instruction!(adc, indexed_indirect),
            0x65 => instruction!(adc, zeropage),
            0x69 => instruction!(adc, immediate),
            0x6d => instruction!(adc, absolute),
            0x70 => instruction!(bvs, relative, overflow, true),
            0x71 => instruction!(adc, indirect_indexed, true),
            0x75 => instruction!(adc, zeropage_x),
            0x7d => instruction!(adc, absolute_x, true),
            0x79 => instruction!(adc, absolute_y),
            0x90 => instruction!(bcc, relative, carry, false),
            0xb0 => instruction!(bcs, relative, carry, true),
            0xd0 => instruction!(bne, relative, zero, false),
            0xf0 => instruction!(beq, relative, zero, true),
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
     * With immediate addressing, the byte after the opcode is used directly as
     * operand.
     */
    async fn immediate(&mut self, bus: &impl Bus) -> u16 {
        self.program_counter = self.program_counter.wrapping_add(1);
        self.program_counter.wrapping_sub(1)
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
     * Absolute indexed addressing adds an index register to the absolute
     * address. Might take an additional cycle depending on the instruction and
     * whether or not a page boundary is crossed.
     */
    async fn absolute_x(&mut self, bus: &impl Bus) -> (u16, bool) {
        let address = self.absolute(bus).await;
        let effective_address = address.wrapping_add(self.x as u16);

        if address.high_byte() != effective_address.high_byte() {
            (effective_address, true)
        } else {
            (effective_address, false)
        }
    }

    /**
     * Absolute indexed addressing adds an index register to the absolute
     * address. Can take an additional cycle depending on the instruction and
     * whether or not a page boundary is crossed.
     */
    async fn absolute_y(&mut self, bus: &impl Bus) -> u16 {
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
    async fn zeropage(&mut self, bus: &impl Bus) -> u16 {
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
    async fn indirect_indexed(&mut self, bus: &impl Bus) -> (u16, bool) {
        let zeropage_address = self.zeropage(bus).await;
        let low_byte = self.read(bus, zeropage_address).await;
        let high_byte = self.read(bus, zeropage_address.wrapping_add(1)).await;
        let address = u16::from_bytes(low_byte, high_byte);
        let effective_address = address.wrapping_add(self.y as u16);

        if address.high_byte() != effective_address.high_byte() {
            (effective_address, true)
        } else {
            (effective_address, false)
        }
    }

    /**
     * Logical AND of the accumulator with a byte of memory. The result is
     * stored in the accumulator.
     */
    async fn and(&mut self, bus: &impl Bus, address: u16) {
        let operand = self.read(bus, address).await;
        self.accumulator &= operand;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
    }

    /**
     * Add with carry.
     */
    async fn adc(&mut self, bus: &impl Bus, address: u16) {
        let operand = self.read(bus, address).await;
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
    async fn asl(&mut self, bus: &impl Bus, address: u16) {
        let operand = self.read(bus, address).await;
        self.clock.advance(1); // Dummy write while doing the operation
        self.status.carry = operand.bit(7);
        let result = operand << 1;
        self.status.zero = result == 0;
        self.status.negative = result.bit(7);
        self.write(bus, address, result).await;
    }

    /**
     * Arithmetic shift left, accumulator
     */
    fn asl_accumulator(&mut self) {
        self.status.carry = self.accumulator.bit(7);
        self.accumulator <<= 1;
        self.status.zero = self.accumulator == 0;
        self.status.negative = self.accumulator.bit(7);
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
                ($instruction:expr, $addressing:expr, $opcode:expr, $cycles:expr) => {{
                    let start = cpu.clock.current();
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
            check_timing!("ADC", "Immediate", 0x69, 2);
            check_timing!("ADC", "Zeropage", 0x65, 3);
            check_timing!("ADC", "Zeropage,X", 0x75, 4);
            check_timing!("ADC", "Absolute", 0x6d, 4);
            check_timing!("ADC", "Absolute,X", 0x7d, 4);
            check_timing!("ADC", "Absolute,Y", 0x79, 4);
            check_timing!("ADC", "(Indirect,X)", 0x61, 6);
            check_timing!("ADC", "(Indirect),Y", 0x71, 5);

            check_timing!("AND", "Immediate", 0x29, 2);
            check_timing!("AND", "Zeropage", 0x25, 3);
            check_timing!("AND", "Zeropage,X", 0x35, 4);
            check_timing!("AND", "Absolute", 0x2d, 4);
            check_timing!("AND", "Absolute,X", 0x3d, 4);
            check_timing!("AND", "Absolute,Y", 0x39, 4);
            check_timing!("AND", "(Indirect,X)", 0x21, 6);
            check_timing!("AND", "(Indirect),Y", 0x31, 5);

            check_timing!("ASL", "Zeropage", 0x06, 5);
            check_timing!("ASL", "Zeropage,X", 0x16, 6);
            check_timing!("ASL", "Absolute", 0x0e, 6);
            check_timing!("ASL", "Absolute,X", 0x1e, 7);

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