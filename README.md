# RuNES
Emulator for the Nintendo Entertainment System, written in Rust.

Current implementation does not yet support arbitrary iNES execution, because of incomplete Cartridge support.
The Ricoh 2A03 variant of the MOS 6502 is fully implemented and cycle-accurate, as verified by nestest.
The full test suite, including nestest, can be carried out by running "cargo test".
Note that the nestest ROM is not provided with this repository and must instead be provided by the user to be able to run all test suites.