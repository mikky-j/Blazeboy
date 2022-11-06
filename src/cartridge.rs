use std::{
    fmt::Display,
    fs::File,
    io::{BufRead, BufReader, Seek},
};

use crate::mbc_mapper::MbcMapper;

pub type Buffer<'a> = &'a mut BufReader<File>;

#[derive(Debug)]
pub enum CartridgeError {
    FileDoesNotExist(String),
    CheckNotPassed(&'static str),
    InvalidLicenseCode(u8),
    InvalidRamSize(u8),
    InvalidRomSize(u8),
    InvalidCatridgeType(u8),
    // WrongBufferStartPosition(u64, u64),
    UnexpectedEndOfCartridge(usize, usize),
    UnexpectedMbc(CatridgeType),
    UnexpectedMbcAddress(CatridgeType, usize),
    MbcRamDisabled(CatridgeType),
}

impl Display for CartridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CartridgeError::*;
        match self {
            FileDoesNotExist(path) => format!("File does not exist at specified path `{path}`.\nPlease provide correct path"),
            CheckNotPassed(a) => format!( "{}", *a),
            InvalidLicenseCode(byte) => format!("An invalid license code `{byte}`"),
            InvalidRamSize(byte) => format!("An invalid byte `{byte}` was given as the ram size"),
            InvalidRomSize(byte) => format!("An invalid byte `{byte}` was given as the rom size"),
            InvalidCatridgeType(byte) => format!("An invalid byte `{byte} was given as the catridge type"),
            UnexpectedEndOfCartridge(start, end) => format!("Reached the unexpected end of the file when trying to read from {start:04x} to {end:04x}"),
            UnexpectedMbc(mbc) => format!("Unexpected Mbc: `{mbc}` was found"),
            UnexpectedMbcAddress(mbc,address) => format!("Unexpected {mbc} Address `{address}` was passed in"),
            MbcRamDisabled(mbc) => format!("Ram is disabled for {mbc}")
            // WrongBufferStartPosition(expected,got ) => write!(f,"Expected start location to be `{expected:04x}` but got `{got:04x}`"),
        }.fmt(f)
    }
}

#[derive(Debug)]
pub struct Cartridge {
    pub entry_point: Vec<u8>,
    pub title: String,
    pub cgb_only: bool,
    pub license_code: u8,
    pub sgb: bool,
    pub mapper: Option<MbcMapper>,
    pub cartrigde_type: Vec<CatridgeType>,
    pub rom_size: usize,
    pub ram_size: usize,
    pub japanese: bool,
    pub contents: BufReader<File>,
}

pub fn read(buffer: Buffer, start_pos: usize, capacity: usize) -> Result<Vec<u8>, CartridgeError> {
    let buffer_pos = buffer.stream_position().unwrap() as i64;
    buffer
        .seek_relative(start_pos as i64 - buffer_pos)
        .map_err(|_| CartridgeError::UnexpectedEndOfCartridge(start_pos, start_pos + capacity))?;
    if buffer.buffer().is_empty() {
        buffer.fill_buf().unwrap();
    }
    let mut result = Vec::with_capacity(capacity);
    unsafe { result.set_len(capacity) }
    let data = buffer.buffer();
    if data.len() == 0 {
        return Err(CartridgeError::UnexpectedEndOfCartridge(
            start_pos,
            start_pos + capacity,
        ));
    }
    result.copy_from_slice(&data[..capacity]);
    buffer
        .seek_relative(capacity as i64)
        .map_err(|_| CartridgeError::UnexpectedEndOfCartridge(start_pos, start_pos + capacity))?;
    Ok(result)
}

impl Cartridge {
    const NINTENDO_LOGO: [u8; 48] = [
        0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00,
        0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD,
        0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB,
        0xB9, 0x33, 0x3E,
    ];

    fn get_entry_point(buffer: Buffer) -> Result<Vec<u8>, CartridgeError> {
        let result = read(buffer, 0x100, 4)?;
        Ok(result)
    }

    fn validate_logo(buffer: Buffer) -> Result<bool, CartridgeError> {
        let logo = read(buffer, 0x104, 48)?;
        let valid = logo.eq(&Self::NINTENDO_LOGO);
        Ok(valid)
    }

    fn get_title(buffer: Buffer) -> Result<String, CartridgeError> {
        let title = read(buffer, 0x134, 16)?;
        Ok(title.iter().map(|c| *c as char).collect::<String>())
    }

    fn get_cgb_flag(buffer: Buffer) -> Result<bool, CartridgeError> {
        let cgb = read(buffer, 0x143, 1)?;
        let valid = cgb[0] == 0xC0;
        Ok(valid)
    }

    fn get_license_code(buffer: Buffer) -> Result<u8, CartridgeError> {
        let license_code = read(buffer, 0x144, 2)?;
        Ok(license_code[0])
    }

    fn get_sgb(buffer: Buffer) -> Result<bool, CartridgeError> {
        let sgb = read(buffer, 0x146, 1)?;
        Ok(sgb[0] == 0x3)
    }

    fn get_cartridge_type(buffer: Buffer) -> Result<u8, CartridgeError> {
        let cart = read(buffer, 0x147, 1)?;
        match cart[0] {
            0x0..=0x9 | 0xB..=0xD | 0xf..=0x13 | 0x19..=0x1e | 0x20 | 0x22 | 0xfc..=0xff => {
                Ok(cart[0])
            }
            _ => Err(CartridgeError::InvalidCatridgeType(cart[0])),
        }
    }

    fn get_rom_size(buffer: Buffer) -> Result<usize, CartridgeError> {
        let rom_size = read(buffer, 0x148, 1)?;
        match rom_size[0] {
            0x0..=0x8 | 0x52..=0x54 => Ok(32 << rom_size[0]),
            _ => Err(CartridgeError::InvalidRomSize(rom_size[0])),
        }
    }

    fn get_ram_size(buffer: Buffer) -> Result<usize, CartridgeError> {
        let ram_size = read(buffer, 0x149, 1)?;
        match ram_size[0] {
            0x0..=0x5 => Ok(ram_size[0] as usize),
            _ => Err(CartridgeError::InvalidRamSize(ram_size[0])),
        }
    }

    fn get_destination_code(buffer: Buffer) -> Result<bool, CartridgeError> {
        let japanese = read(buffer, 0x14a, 1)?;
        Ok(japanese[0] == 0x1)
    }

    fn validate_checksum(buffer: Buffer) -> Result<bool, CartridgeError> {
        let checksum = read(buffer, 0x134, 0x14c - 0x134)?;
        let value = checksum.iter().fold(0, |acc, x| acc as i32 - *x as i32 - 1);
        Ok(value & 0xFF != 0)
    }

    pub fn new(filename: &str) -> Result<Self, CartridgeError> {
        let file = File::open(filename)
            .map_err(|_| CartridgeError::FileDoesNotExist(format!("{filename}")))?;
        let mut buffer = BufReader::with_capacity(4096, file);
        let entry_point = Self::get_entry_point(&mut buffer)?;
        let title = Self::get_title(&mut buffer)?;
        let japanese = Self::get_destination_code(&mut buffer)?;
        let valid_logo = Self::validate_logo(&mut buffer)?;
        if !valid_logo {
            return Err(CartridgeError::CheckNotPassed(
                "Rom did not contain a valid nintendo logo",
            ));
        }
        let cgb = Self::get_cgb_flag(&mut buffer)?;
        let license_code = Self::get_license_code(&mut buffer)?;
        let sgb_flag = Self::get_sgb(&mut buffer)?;
        let cartridge_type = Self::get_cartridge_type(&mut buffer)?;
        let rom_size = Self::get_rom_size(&mut buffer)?;
        let ram_size = Self::get_ram_size(&mut buffer)?;
        let valid_checksum = Self::validate_checksum(&mut buffer)?;
        let cartridge_type = CatridgeType::get_type(cartridge_type)?;
        let mbc = CatridgeType::get_mbc(cartridge_type[0]);

        let mapper = if let Some(mbc) = mbc {
            Some(MbcMapper::new(mbc, rom_size, ram_size > 0))
        } else {
            None
        };
        if !valid_checksum {
            return Err(CartridgeError::CheckNotPassed("Checksum not passed"));
        }
        Ok(Cartridge {
            title,
            entry_point,
            ram_size,
            rom_size,
            japanese,
            mapper,
            cartrigde_type: cartridge_type,
            cgb_only: cgb,
            license_code,
            sgb: sgb_flag,
            contents: buffer,
        })
    }

    pub fn cart_read(&mut self, address: u16) -> Result<u8, CartridgeError> {
        let effective_addresss = match address {
            0x0000..=0x3FFF => address as usize,
            0x4000..=0x7FFF => {
                if let Some(mapper) = &self.mapper {
                    mapper.get_read_address(address)?
                } else {
                    return Ok(0xFF);
                }
            }
            _ => panic!("Address is out of range"),
        };
        let contents = read(&mut self.contents, effective_addresss, 1)?;
        Ok(contents[0])
    }

    pub fn cart_write(&mut self, address: u16, value: u8) {
        if let Some(mapper) = &mut self.mapper {
            mapper.handle_write(address, value);
        }
    }

    /// I think this function works but I can't really test it out coz I can't find any roms that use multicart
    /// Todo: verify the validity of this function
    fn _check_if_multicart<'b>(contents: Buffer<'b>) -> bool {
        let start_locations = [0x104, 0x40104, 0x80104, 0xC0104];
        let mut result = 0;

        for location in start_locations {
            let checksum = match read(contents, location, 48) {
                Ok(value) => value,
                Err(e) => {
                    println!("Error: {e}");
                    break;
                }
            };
            result += checksum.eq(&Self::NINTENDO_LOGO) as u8;
        }
        println!("result: {result}");
        result >= 2
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum CatridgeType {
    Rom,
    Mbc1,
    Mbc2,
    Mbc3,
    Ram,
    Battery,
    Timer,
    Mmm01,
    Mbc5,
    Mbc6,
    Rumble,
    Sensor,
    Camera,
    Tama5,
    HuC3,
    HuC1,
    Mbc7,
}

impl Display for CatridgeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CatridgeType::*;
        let value = match *self {
            Rom => "Rom",
            Mbc1 => "Mbc1",
            Mbc2 => "Mbc2",
            Mbc3 => "Mbc3",
            Ram => "Ram",
            Battery => "Battery",
            Timer => "Timer",
            Mmm01 => "Mmm01",
            Mbc5 => "Mbc5",
            Mbc6 => "Mbc6",
            Rumble => "Rumble",
            Sensor => "Sensor",
            Camera => "Camera",
            Tama5 => "Tama5",
            HuC3 => "HuC3",
            HuC1 => "HuC1",
            Mbc7 => "Mbc7",
        };
        value.fmt(f)
    }
}

impl CatridgeType {
    pub fn get_type(data: u8) -> Result<Vec<CatridgeType>, CartridgeError> {
        let mut result = match data {
            0x00 => vec![CatridgeType::Rom],
            0x01 => vec![CatridgeType::Mbc1],
            0x02 => vec![CatridgeType::Mbc1, CatridgeType::Ram],
            0x03 => vec![CatridgeType::Mbc1, CatridgeType::Ram, CatridgeType::Battery],
            0x05 => vec![CatridgeType::Mbc2],
            0x06 => vec![CatridgeType::Mbc2, CatridgeType::Battery],
            0x08 => vec![CatridgeType::Rom, CatridgeType::Ram],
            0x09 => vec![CatridgeType::Rom, CatridgeType::Ram, CatridgeType::Battery],
            0x0B => vec![CatridgeType::Mmm01],
            0x0C => vec![CatridgeType::Mmm01, CatridgeType::Ram],
            0x0D => vec![
                CatridgeType::Mmm01,
                CatridgeType::Ram,
                CatridgeType::Battery,
            ],
            0x0F => vec![
                CatridgeType::Mbc3,
                CatridgeType::Timer,
                CatridgeType::Battery,
            ],
            0x10 => vec![
                CatridgeType::Mbc3,
                CatridgeType::Timer,
                CatridgeType::Ram,
                CatridgeType::Battery,
            ],
            0x11 => vec![CatridgeType::Mbc3],
            0x12 => vec![CatridgeType::Mbc3, CatridgeType::Ram],
            0x13 => vec![CatridgeType::Mbc3, CatridgeType::Ram, CatridgeType::Battery],
            0x19 => vec![CatridgeType::Mbc5],
            0x1a => vec![CatridgeType::Mbc5, CatridgeType::Ram],
            0x1b => vec![CatridgeType::Mbc5, CatridgeType::Ram, CatridgeType::Battery],
            0x1c => vec![CatridgeType::Mbc5, CatridgeType::Rumble],
            0x1d => vec![CatridgeType::Mbc5, CatridgeType::Ram, CatridgeType::Rumble],
            0x1e => vec![
                CatridgeType::Mbc5,
                CatridgeType::Ram,
                CatridgeType::Rumble,
                CatridgeType::Battery,
            ],
            0x20 => vec![CatridgeType::Mbc6],
            0x22 => vec![
                CatridgeType::Mbc7,
                CatridgeType::Ram,
                CatridgeType::Rumble,
                CatridgeType::Battery,
                CatridgeType::Sensor,
            ],
            0xfc => vec![CatridgeType::Camera],
            0xfd => vec![CatridgeType::Tama5],
            0xfe => vec![CatridgeType::HuC3],
            0xff => vec![CatridgeType::HuC1, CatridgeType::Ram, CatridgeType::Battery],
            _ => return Err(CartridgeError::InvalidCatridgeType(data)),
        };
        result.shrink_to_fit();
        Ok(result)
    }

    fn get_mbc(data: CatridgeType) -> Option<CatridgeType> {
        // I am going to do something tricky here, since in the `get_type` function I made sure that all MBCs are the
        // first element of the list I can just pass the first element and check if it an mbc and return itself
        use CatridgeType::*;
        match data {
            Mbc1 | Mbc2 | Mbc3 | Mbc5 | Mbc6 | Mbc7 => Some(data),
            _ => None,
        }
    }
}

#[cfg(test)]
mod cart_test {
    use super::*;
    #[test]
    fn test_read() {
        let mut buffer = BufReader::new(File::open("src/roms/cpu_instrs.gb").unwrap());
        let data = match read(&mut buffer, 0x144, 19) {
            Ok(v) => v,
            Err(v) => {
                println!("{v}");
                return;
            }
        };
        let second_data = match read(&mut buffer, 0x100, 12) {
            Ok(v) => v,
            Err(v) => {
                println!("{v}");
                return;
            }
        };
        println!("{data:?}");
        println!("{second_data:?}");
        // println!("{:04x}", (0x144u64 + 19).checked_sub(0x100).unwrap())
    }
    #[test]
    fn test_load() {
        let cartridge = match Cartridge::new("src/roms/cpu_instrs.gb") {
            Ok(v) => v,
            Err(v) => {
                println!("{v}");
                return;
            }
        };
        println!("{:?}", cartridge);
    }

    #[test]
    // Todo: Remove this after you have verified that the function works
    #[should_panic]
    fn test_multicart() {
        let mut cartridge =
            Cartridge::new("./src/roms/Pokemon Red-Blue 2-in-1 (Unl) [S].gb").unwrap();
        let multicart = Cartridge::_check_if_multicart(&mut cartridge.contents);
        assert_eq!(multicart, true);
    }
}
