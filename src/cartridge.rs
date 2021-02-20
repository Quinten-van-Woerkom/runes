/**
 * cartridge.rs
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

use crate::bitwise::Bitwise;
use std::cell::Cell;
use std::io::Read;
use std::fs::File;

/**
 * Cartridges are an often-used hardware expansion point for the NES, as it
 * allowed games to pack their own custom hardware. These custom cartridges
 * could map additional memory, or even pack additional processors. This means
 * that the cartridge is best represented as a polymorphic type.
 * 
 * Additionally, separate processing power emulation would also require
 * separate synchronization for the cartridge, this is to be implemented later.
 */
pub trait Cartridge {
    fn cpu_read(&self, address: u16) -> u8;
    fn cpu_write(&self, address: u16, value: u8);
    fn ppu_read(&self, address: u16) -> u8;
    fn ppu_write(&self, address: u16, value: u8);
}


/**
 * Loads a cartridge from an NES 2.0 file.
 * Can fail if the file cannot be read, if the file does not contain a validly
 * formatted NES 2.0 file, or if the mapper type is not (yet) supported.
 */
pub fn load_cartridge(path: &str) -> Result<Box<dyn Cartridge>, std::io::Error> {
    let mut file = File::open(path)?;
    let header = Header::load(&mut file)?;

    match header.mapper() {
        0 => unimplemented!(),
        _ => Err(std::io::Error::new(std::io::ErrorKind::Other, "Trying to load unsupported mapper"))
    }
}


/**
 * Each NES 2.0 file starts with a 16-byte header that identifies the file
 * format and mapper type. This header must be loaded and decoded before the
 * virtual cartridge can be constructed.
 * 
 * Decoding is done lazily, since the data used depends on the ROM loaded and
 * mapper type used.
 */
struct Header {
    bytes: [u8; 16]
}

impl Header {
    /**
     * Validates that the file contains an NES file, by checking the "magic
     * number" at its start, which should contain "NES<EOF>".
     */
    fn load(file: &mut File) -> Result<Header, std::io::Error> {
        let mut bytes = [0u8; 16];
        file.read_exact(&mut bytes)?;

        if bytes[0..4] != [0x4E, 0x45, 0x53, 0x1A] {
            Err(std::io::Error::new(std::io::ErrorKind::InvalidData, ".nes files must start with NES<EOF>"))
        } else {
            Ok(Header{bytes})
        }
    }

    /**
     * Returns the mapper type contained in the loaded ROM file.
     */
    fn mapper(&self) -> usize {
        let low_nybble = self.bytes[6].bits(4, 4) as usize;
        let middle_nybble = self.bytes[7].bits(4, 4) as usize;
        let high_nybble = self.bytes[8].bits(0, 4) as usize;
        low_nybble | middle_nybble << 4 | high_nybble << 8
    }

    /**
     * Some games were made specifically for the NTSC or PAL territories and
     * depend on the resulting CPU/PPU timings. This must then also be encoded
     * in the ROM file, to allow the emulator to switch.
     */
    fn timing_mode(&self) -> TimingMode {
        match self.bytes[12].bits(0, 2) {
            0 => TimingMode::Ntsc,
            1 => TimingMode::Pal,
            2 => TimingMode::MultipleRegion,
            3 => TimingMode::Dendy,
            _ => unreachable!() // 2-bit values can never exceed 3
        }
    }

    /**
     * Some custom ROMs contain a 512-byte trainer straight after the header,
     * often for compatibility purposes.
     */
    fn trainer_size(&self) -> usize {
        match self.bytes[6].bit(2) {
            false => 0,
            true => 512,
        }
    }

    /**
     * The PRG-ROM size is stored in one of two formats:
     * - Simply the number of 16 KiB units.
     * - An exponent-multiplier variant, for sizes not representable by the
     *   above notation.
     * Some Vs. Systems follow another convention, but that is not currently
     * implemented.
     */
    fn prg_rom_size(&self) -> usize {
        let low_byte = self.bytes[4];
        let high_byte = self.bytes[9].bits(0, 4);

        if high_byte != 0xf {
            (low_byte as usize | ((high_byte as usize) << 8)) * 16384
        } else {
            let multiplier = 2*low_byte.bits(0, 2) as usize + 1;
            let exponent = 2usize.pow(low_byte.bits(2, 6).into());
            multiplier * exponent
        }
    }

    /**
     * The CHR-ROM size is stored in one of two formats:
     * - Simply the number of 16 KiB units.
     * - An exponent-multiplier variant, for sizes not representable by the
     *   above notation.
     * Similar to the PRG-ROM size format.
     */
    fn chr_rom_size(&self) -> usize {
        let low_byte = self.bytes[5];
        let high_byte = self.bytes[9].bits(4, 4);

        if high_byte != 0xf {
            (low_byte as usize | ((high_byte as usize) << 8)) * 8192
        } else {
            let multiplier = 2*low_byte.bits(0, 2) as usize + 1;
            let exponent = 2usize.pow(low_byte.bits(2, 6).into());
            multiplier * exponent
        }
    }

    /**
     * The PRG-RAM size is stored in terms of a shift count, such that its size
     * is <64 << shift count> bytes.
     */
    fn prg_ram_size(&self) -> usize {
        let shift = self.bytes[10].bits(0, 4);
        match shift {
            0 => 0,
            _ => 64 << shift
        }
    }

    /**
     * The PRG-NVRAM size is stored in terms of a shift count, such that its
     * size is <64 << shift count> bytes.
     */
    fn prg_nvram_size(&self) -> usize {
        let shift = self.bytes[10].bits(4, 4);
        match shift {
            0 => 0,
            _ => 64 << shift
        }
    }

    /**
     * The CHR-RAM size is stored in terms of a shift count, such that its size
     * is <64 << shift count> bytes.
     */
    fn chr_ram_size(&self) -> usize {
        let shift = self.bytes[11].bits(0, 4);
        match shift {
            0 => 0,
            _ => 64 << shift
        }
    }

    /**
     * The CHR-NVRAM size is stored in terms of a shift count, such that its
     * size is <64 << shift count> bytes.
     */
    fn chr_nvram_size(&self) -> usize {
        let shift = self.bytes[11].bits(4, 4);
        match shift {
            0 => 0,
            _ => 64 << shift
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum TimingMode {
    Ntsc,
    Pal,
    MultipleRegion,
    Dendy,
}


/**
 * Convenience function to read exactly <len> bytes from a file to create a new
 * vector.
 */
fn read_exact_to_vec(file: &mut File, len: usize) -> Result<Vec<u8>, std::io::Error> {
    let mut result = Vec::with_capacity(len);
    unsafe { result.set_len(len) } // Safe because u8 is Copy
    file.read_exact(result.as_mut_slice())?;
    Ok(result)
}


/**
 * Mapper 0, or NROM, can be used to represents cartridges boards NES-NROM-128,
 * NES-NROM-256, and counterparts.
 * It is one of the most basic cartridge types.
 */
struct Mapper0 {
    header: Header,
    prg_ram: Vec<Cell<u8>>,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

impl Mapper0 {
    /**
     * Loading mapper 0 can fail, if the file does not contain sufficient data,
     * or incorrectly formatted data.
     * Preconditions:
     *  1. The header should correspond to the passed file.
     *  2. The mapper corresponding to the file and header should be mapper 0.
     */
    fn load(header: Header, file: &mut File) -> Result<Self, std::io::Error> {
        assert!(header.mapper() == 0, "Cannot construct NROM cartridge from different mapper");
        
        let prg_ram = {
            let len = header.prg_ram_size();
            let mut result = Vec::with_capacity(len);
            unsafe { result.set_len(len); } // Safe because u8 is Copy

            let trainer_size = header.trainer_size();
            if len < 0x1000 && trainer_size != 0 {
                return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, 
                "ROM has a trainer but not sufficient PRG-RAM to write it to."))
            }
            
            match trainer_size {
                0 => (),
                _ => file.read_exact(&mut result[0x1000..0x1000 + trainer_size])?
            };

            let mut manual_drop = std::mem::ManuallyDrop::new(result);
            let pointer = manual_drop.as_mut_ptr();
            let len = manual_drop.len();
            let capacity = manual_drop.capacity();

            // Safe because Cell<u8> has same alignment and size as u8
            unsafe { Vec::from_raw_parts(pointer as *mut Cell<u8>, len, capacity) }
        };

        let prg_rom = read_exact_to_vec(file, header.prg_rom_size())?;
        let chr_rom = read_exact_to_vec(file, header.chr_rom_size())?;

        Ok(Mapper0 { header, prg_ram, prg_rom, chr_rom })
    }
}

impl Cartridge for Mapper0 {
    fn cpu_read(&self, address: u16) -> u8 {
        match address {
            0x6000..=0x7fff => {
                let address = (address as usize - 0x6000) % self.prg_ram.len();
                self.prg_ram[address].get()
            },
            0x8000..=0xffff => {
                let address = (address as usize - 0x8000) % self.prg_rom.len();
                self.prg_rom[address]
            },
            _ => unreachable!()
        }
    }

    fn cpu_write(&self, address: u16, value: u8) {
        match address {
            0x6000..=0x7fff => {
                let address = (address as usize - 0x6000) % self.prg_ram.len();
                self.prg_ram[address].set(value);
            },
            0x8000..=0xffff => {},
            _ => unreachable!()
        }
    }

    fn ppu_read(&self, _address: u16) -> u8 { 0 }
    fn ppu_write(&self, _address: u16, _value: u8) {}
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn header_loading() {
        let mut file = File::open("Donkey Kong (World) (Rev A).nes").unwrap();
        let header = Header::load(&mut file).unwrap();

        assert_eq!(header.prg_rom_size(), 16384);
        assert_eq!(header.chr_rom_size(), 8192);
        assert_eq!(header.prg_ram_size(), 0);
        assert_eq!(header.chr_ram_size(), 0);
        assert_eq!(header.prg_nvram_size(), 0);
        assert_eq!(header.chr_nvram_size(), 0);
        assert_eq!(header.timing_mode(), TimingMode::Ntsc)
    }

    #[test]
    fn mapper0_loading() {
        let mut file = File::open("Donkey Kong (World) (Rev A).nes").unwrap();
        let header = Header::load(&mut file).unwrap();
        let cartridge = Mapper0::load(header, &mut file).unwrap();

        assert_eq!(cartridge.prg_rom.len(), 16384);
        assert_eq!(cartridge.chr_rom.len(), 8192);
    }
}