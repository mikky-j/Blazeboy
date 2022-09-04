use std::{fs::File, io::{BufReader, Seek, BufRead}, fmt::Display};


type Buffer<'a> = &'a mut BufReader<File>;

#[derive(Debug)]
pub enum CartridgeError {
    FileDoesNotExist(String),
    CheckNotPassed(&'static str),
    InvalidLicenseCode(u8),
    InvalidRamSize(u8),
    InvalidRomSize(u8),
    InvalidCatridgeType(u8),
    // WrongBufferStartPosition(u64, u64),
    UnexpectedEndOfCartridge(usize, usize)
}



impl Display for CartridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CartridgeError::*;
        match self {
            FileDoesNotExist(path) => write!(f,"File does not exist at specified path `{path}`.\nPlease provide correct path"),
            CheckNotPassed(a) => write!(f, "{}", *a),
            InvalidLicenseCode(byte) => write!(f,"An invalid license code `{byte}`"),
            InvalidRamSize(byte) => write!(f,"An invalid byte `{byte}` was given as the ram size"),
            InvalidRomSize(byte) => write!(f,"An invalid byte `{byte}` was given as the rom size"),
            InvalidCatridgeType(byte) => write!(f,"An invalid byte `{byte} was given as the catridge type"),
            UnexpectedEndOfCartridge(start, end) => write!(f,"Reached the unexpected end of the file when trying to read from {start:04x} to {end:04x}"),
            // WrongBufferStartPosition(expected,got ) => write!(f,"Expected start location to be `{expected:04x}` but got `{got:04x}`"),
        }
    }


}

#[derive(Debug)]
pub struct Cartridge {
    pub entry_point: Vec<u8>,
    pub title: String,
    pub cgb_only: bool,
    pub license_code: u8,
    pub sgb: bool,
    pub cartrigde_type: u8,
    pub rom_size: usize,
    pub ram_size: usize,
    pub japanese: bool,
    pub contents: BufReader<File>,
}



pub fn read(buffer: Buffer, start_pos: usize, capacity: usize) -> Result<Vec<u8>, CartridgeError> {
    let buffer_pos = buffer.stream_position().unwrap() as i64;
    buffer.seek_relative( start_pos as i64 - buffer_pos).map_err(|_| CartridgeError::UnexpectedEndOfCartridge(start_pos, start_pos + capacity))?;
    if buffer.buffer().is_empty() {
        buffer.fill_buf().unwrap();
    }
    let mut result = Vec::with_capacity(capacity);
    unsafe {result.set_len(capacity)}
    let data = buffer.buffer();
    result.copy_from_slice(&data[..capacity]);
    buffer.seek_relative(capacity as i64).map_err(|_| CartridgeError::UnexpectedEndOfCartridge(start_pos, start_pos + capacity) )?;
    Ok(result)
}

impl Cartridge {
    const NINTENDO_LOGO: [u8; 48] = [
        0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
        0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
        0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
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

    fn get_title(buffer: Buffer) -> Result<String, CartridgeError>{
        let title = read(buffer, 0x134, 16)?;
        Ok(title.iter().map(
            |c| *c as char,
        ).collect::<String>())
    }

    fn get_cgb_flag(buffer: Buffer) -> Result<bool, CartridgeError> {
        let  cgb = read(buffer, 0x143, 1)?;
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
            0x0..=0x9 | 0xB..=0xD | 0xf..=0x13 | 0x19..=0x1e| 0x20 | 0x22 | 0xfc..=0xff => Ok(cart[0]),
            _ => Err(CartridgeError::InvalidCatridgeType(cart[0]))
        }
    }

    fn get_rom_size(buffer: Buffer) -> Result<usize, CartridgeError> {
        let rom_size = read(buffer, 0x148, 1)?;
        match rom_size[0] {
            0x0..=0x8 | 0x52..=0x54 => Ok(32 << rom_size[0]),
            _ => Err(CartridgeError::InvalidRomSize(rom_size[0]))
        }
    }

    fn get_ram_size(buffer: Buffer) -> Result<usize, CartridgeError> {
        let ram_size = read(buffer, 0x149, 1)?;
        match ram_size[0] {
            0x0..=0x5=> Ok(ram_size[0] as usize ),
            _ => Err(CartridgeError::InvalidRamSize(ram_size[0]))
        }
    }

    fn get_destination_code(buffer: Buffer) -> Result<bool, CartridgeError> {
        let japanese = read(buffer, 0x14a, 1)?;
        Ok(japanese[0] == 0x1)
    }

    fn validate_checksum(buffer: Buffer) -> Result<bool, CartridgeError> {
        let checksum = read(buffer, 0x134, 0x14c-0x134)?;
        let value = checksum.iter().fold(0, |acc, x| acc as i32 - *x as i32 - 1);
        Ok(value & 0xFF != 0)
    }


    pub fn new(filename: &str) -> Result<Self, CartridgeError> {
        let file = File::open(filename).map_err(|_| CartridgeError::FileDoesNotExist(format!("{filename}")))?;
        let mut buffer = BufReader::with_capacity(4096, file);
        let entry_point = Self::get_entry_point(&mut buffer)?;
        let title = Self::get_title(&mut buffer)?;
        let japanese = Self::get_destination_code(&mut buffer)?;
        let valid_logo = Self::validate_logo(&mut buffer)?;
        if !valid_logo {
            return Err(CartridgeError::CheckNotPassed("Rom did not contain a valid nintendo logo"));
        }
        let cgb = Self::get_cgb_flag(&mut buffer)?;
        let license_code = Self::get_license_code(&mut buffer)?;
        let sgb_flag = Self::get_sgb(&mut buffer)?;
        let cartrige_type = Self::get_cartridge_type(&mut buffer)?;
        let rom_size = Self::get_rom_size(&mut buffer)?;
        let ram_size = Self::get_ram_size(&mut buffer)?;
        let valid_checksum = Self::validate_checksum(&mut buffer)?;
        if !valid_checksum {
            return Err(CartridgeError::CheckNotPassed(
                "Checksum not passed"
            ))
        }
            Ok(
                Cartridge {
                    title,
                    entry_point,
                    ram_size,
                    rom_size,
                    japanese,
                    cartrigde_type: cartrige_type,
                    cgb_only: cgb,
                    license_code,
                    sgb: sgb_flag,
                    contents: buffer

                }

            )
    }

    pub fn cart_read(&mut self, address: u16) -> Result<u8, CartridgeError> {
        let contents = read(&mut self.contents, address as usize, 1)?;
        Ok(contents[0])
    }

    pub fn cart_write(&mut self, _address: u16, _value: u8) {
        // todo!("Need to handle rom banking");
        println!("Unsupported writing to {}", _address);
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
            Err(v) => {println!("{v}"); return;}
        };
        let second_data = match read(&mut buffer, 0x100, 12) {
            Ok(v) => v,
            Err(v) => {println!("{v}"); return;}
        };
        println!("{data:?}");
        println!("{second_data:?}");
        // println!("{:04x}", (0x144u64 + 19).checked_sub(0x100).unwrap())

    }
    #[test]
    fn test_load() {
        let cartrige = match Cartridge::new("src/roms/cpu_instrs.gb") {
            Ok(v) => v,
            Err(v) => {println!("{v}"); return;}
        };
        println!("{:?}", cartrige);
    }

}