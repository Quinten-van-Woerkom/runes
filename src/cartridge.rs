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

use std::fs::File;

/**
 * Cartridges are an often-used hardware expansion point for the NES, as it
 * allowed games to pack their own custom hardware. These custom cartridges
 * could map additional memory, or even pack additional processors.
 * This means that  
 */
pub trait Cartridge {
}


/**
 * Loads a cartridge from an NES 2.0 file.
 * Can fail only if the file doesn't exist, can't open, etc. or if it doesn't
 * contain a validly-formatted NES 2.0 file.
 */
pub fn load_cartridge(path: &str) -> Result<Box<Cartridge>, std::io::Error> {
    let file = std::fs::File::open(path)?;

}


/**
 * Each NES 2.0 file starts with a 16-byte header that identifies the file
 * format and mapper type. This header must be loaded and decoded before the
 * virtual cartridge can be constructed.
 * 
 * Note: Some fields, such as extended console types, are ignored in this
 * implementation.
 */
struct Header {
    mapper: usize,
    submapper: u8,
    console_type: ConsoleType,
    prg_rom_size: usize,
    chr_rom_size: usize,
    nametable_mirroring: NametableMirroring,
    non_volatile_memory_present: bool,
    trainer_present: bool,
    four_screen_mode: bool,
    prg_ram_size: usize,
    prg_nvram_size: usize,
    chr_ram_size: usize,
    chr_nvram_size: usize,
    timing_mode: TimingMode,
}

enum NametableMirroring {
    HorizontalOrMapperControlled,
    Vertical,
}

/**
 * The following console types can be encoded by NES 2.0 files.
 * Emulator behaviour is not altered based on this, the console type in a ROM
 * is only decoded for debugging purposes.
 */
enum ConsoleType {
    NesOrFamicom,
    VsSystem,
    Playchoice10,
    Extended,
}

/**
 * Some games were made specifically for the NTSC or PAL territories and depend
 * on the resulting CPU/PPU timings. This must then also be encoded in the ROM.
 */
enum TimingMode {
    Ntsc,
    Pal,
    MultipleRegion,
    Dendy,
}