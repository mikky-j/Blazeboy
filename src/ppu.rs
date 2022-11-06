use crate::{
    clear_bits, get_bit, interrupt::InterruptSource, ppu_registers::PPURegisters, InterruptRef,
    Wrapper,
};
use std::fmt::Display;

/// This is an Error enum that can hold all possible PPUErrors
pub enum PPUError {
    /// This error can occur when you try to access an invalid line<br>
    /// For example: When you try to acesss line 9 for an 8x8 pixel
    TileError(String),
    /// This error occurs when you try to get a color value that is greater than 3
    ColorError(String),
    /// This error occurs when you try to get the `PPUMode` from a value greater that 3
    PPUModeError(String),
}

impl Display for PPUError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ColorError(v) => v.fmt(f),
            Self::TileError(v) => v.fmt(f),
            Self::PPUModeError(v) => v.fmt(f),
        }
    }
}

pub type PPUResult<T> = Result<T, PPUError>;

// #[derive(Clone, Copy)]
/// This is an enum that represents each Color that the gameboy can showk
enum Color {
    /// This represents color value: `0b00`
    Off,
    /// This represents color value: `0b01`
    LowOn,
    /// This represents color value: `0b10`
    HiOn,
    /// This represents color value: `0b11`
    On,
}

impl Color {
    /// This function returns the color value from a `u8` value<br>
    /// It throws a PPUError::Color if an invalid value is passed into the function
    pub fn from_data(color: u8) -> PPUResult<Self> {
        use Color::*;
        let data = match color {
            0b00 => Off,
            0b01 => LowOn,
            0b10 => HiOn,
            0b11 => On,
            _ => {
                return Err(PPUError::ColorError(format!(
                    "{:08b} is an invalid color",
                    color
                )))
            }
        };
        Ok(data)
    }
}

// #[derive(Default, Clone, Copy)]j
/// This represents the colors for a tile in a `u128`. <br>
/// This struct will be outdated in the future.
struct Tile {
    tile_data: u128,
}

impl Tile {
    /// This returns a new `Tile` from the `tile_number` and from `vram`.
    /// It returns a new tile by using the `tile_number * 16` to find the starting point of a tile
    /// and then it takes the next the 16 bytes and turns it to a  u128 and stores it in the struct
    pub fn new(tile_number: u8, memory: &[u8]) -> Self {
        let start_pos = (tile_number * 16) as usize;
        let mut bytes = [0_u8; 16];
        bytes.copy_from_slice(&memory[start_pos..(start_pos + 16)]);
        Self {
            tile_data: u128::from_be_bytes(bytes),
        }
    }

    /// This is a function that can return the color data for a line in a tile in the form of `u16`<br>
    /// It uses the `line*8` to know the starting bit and then it just takes the next two bytes in the form of a `u16`
    pub fn get_line(&self, line: u8) -> Result<u16, PPUError> {
        if line > 7 {
            return Err(PPUError::TileError(format!(
                "Tried to access line {line} of a sprite"
            )));
        }
        let data = (self.tile_data.reverse_bits() >> (line * 16)) as u16;
        let data = data.to_be_bytes();
        let result = Pixel::transfrom_to_line(data[0], data[1]);
        Ok(result)
    }
}

//? Made this an enum incase I want to also emulate CGB games
/// This is an enm that can represent the current pallete used by a tile.
/// It only has two values but in the future I could add more pallettes and basically emulate Gameboy color games
enum Palette {
    Pallete0,
    Palette1,
}

/// This is the representation of a pixel by a `color` value and the current `palette` for that pixel
struct Pixel {
    color: Color,
    palette: Palette,
}

impl Pixel {
    // pub fn new_empty() -> Self {
    //     Pixel {
    //         color: Color::None,
    //         palette: Palette::None,
    //     }
    // }

    /// This returns a new pixel form the color data a palette and returns a new `Pixel`
    pub fn new(color_data: u8, palette: bool) -> PPUResult<Self> {
        let pixel = Pixel {
            color: Color::from_data(color_data)?,
            palette: if palette {
                Palette::Pallete0
            } else {
                Palette::Palette1
            },
        };
        Ok(pixel)
    }

    /// This function takes two bytes and returns a u16 that represents a line in a tile
    pub fn transfrom_to_line(first_byte: u8, second_byte: u8) -> u16 {
        let mut result = 0;
        for bit_pos in 0..8 {
            result |= (((get_bit!(second_byte, bit_pos) << 1) | get_bit!(first_byte, bit_pos))
                as u16)
                << bit_pos * 2;
        }
        result
    }

    /// This function returns a `u16` representing the pixel data for a line in a tile
    pub fn get_line_bytes(vram: &[u8], tile_number: u8, line: u8) -> PPUResult<u16> {
        if line > 7 {
            return Err(PPUError::TileError(format!(
                "Tried to get pixel data of line {}",
                line
            )));
        }
        let start_pos = ((tile_number as usize) * 16) + line as usize;
        let result = Pixel::transfrom_to_line(vram[start_pos], vram[start_pos + 1]);
        Ok(result)
    }
}

/// This the representation of a `OBJ`<br>
/// It basically stores all the properties of an object that is stored in the `OAM` Ram
struct Sprite {
    pos_x: u8,
    pos_y: u8,
    tile_number: u8,
    bg_priority: bool,
    flip_x: bool,
    flip_y: bool,
    palette: bool,
}

impl Sprite {
    /// This takes a slice from the `OAM` ram and returns a new `Sprite`
    fn new(oam_data: &[u8]) -> Self {
        let pos_y = oam_data[0];
        let pos_x = oam_data[1];
        let tile_number = oam_data[2];
        let bg_priority = get_bit!(oam_data[3], 7) == 1;
        let flip_y = get_bit!(oam_data[3], 6) == 1;
        let flip_x = get_bit!(oam_data[3], 5) == 1;
        let palette = get_bit!(oam_data[3], 4) == 1;
        Sprite {
            pos_x,
            pos_y,
            tile_number,
            bg_priority,
            flip_x,
            flip_y,
            palette,
        }
    }

    /// This function takes the current sprite buffer and can find which sprite should be rendered.<br>
    /// It does this by checking if the `current_x_pos + SCX` and `LY + SCY` is within the range of `sprite.x` and `sprite.y + sprite.height` respectively<br>
    /// ### Note
    /// `current_x_pos` =>  current x postion in the viewport that is being rendered
    // TODO: VERIFY THIS FUNCTION
    pub fn sprite_to_render(
        sprite_buffer: &Vec<Sprite>,
        current_x: u8,
        current_y: u8,
        is_tall_sprite: bool,
    ) -> Option<&Sprite> {
        let sprite_height = if is_tall_sprite { 16 } else { 8 };
        sprite_buffer
            .into_iter()
            .filter(|sprite| {
                let sprite = *sprite;
                current_x >= sprite.pos_x
                    && current_x <= sprite.pos_x + 8
                    && current_y >= sprite.pos_y
                    && current_y <= sprite.pos_y + sprite_height
            })
            .next()
    }

    /// This function returns a line in a sprite object.
    /// This function handles getting the correct line and makes sure that the `flip_x` and `flip_y` properties of the sprite is respected
    pub fn get_line(&self, line: u8, is_sprite_16: bool, vram: &[u8]) -> PPUResult<u16> {
        let tile_number = if is_sprite_16 && line > 8 {
            self.tile_number + 1
        } else {
            self.tile_number
        };
        let tile = Tile::new(tile_number, vram);
        let line = line % 8;
        let mut line_data = tile.get_line(if self.flip_y { 8 - line } else { line })?;
        line_data = if self.flip_x {
            line_data.reverse_bits()
        } else {
            line_data
        };
        let offset = if self.pos_x < 8 { 8 - self.pos_x } else { 0 };
        line_data = line_data.reverse_bits() >> offset;
        Ok(line_data.reverse_bits())
    }
}

/// This is an enum that represents all the possible values of the `LCDC` register
enum LCDCRegisters {
    /// Bit 7 of the lcdc register
    LCDEnable,
    /// Bit 6 of the lcdc register
    WindowTileMapArea,
    /// Bit 5 of the lcdc register
    WindowEnable,
    /// Bit 4 of the lcdc register
    TileDataArea,
    /// Bit 3 of the lcdc register
    TileMapArea,
    /// Bit 2 of the lcdc register
    SpriteSize,
    /// Bit 1 of the lcdc register
    SpriteEnable,
    /// Bit 0 of the lcdc register
    BGPriority,
}

enum PPUMode {
    Oam,
    Vblank,
    Hblank,
    Pushing,
}

impl PPUMode {
    pub fn get_mode(data: u8) -> PPUResult<Self> {
        let mode = match data {
            0b00 => Self::Hblank,
            0b01 => Self::Vblank,
            0b10 => Self::Oam,
            0b11 => Self::Pushing,
            _ => {
                return Err(PPUError::TileError(format!(
                    "Tried to get the PPUMode from value {data}"
                )))
            }
        };
        Ok(mode)
    }

    pub fn get_bit_value(&self) -> u8 {
        match self {
            PPUMode::Oam => 2,
            PPUMode::Vblank => 1,
            PPUMode::Hblank => 0,
            PPUMode::Pushing => 3,
        }
    }
}

enum LCDInterruptSource {
    LyInterrupt,
    OamInterrupt,
    VblankInterrupt,
    HblankInterrupt,
}

/// This is the main PPU struct that holds all the register and internal values
pub struct Ppu {
    registers: Wrapper<PPURegisters>,
    bg_buffer: u16,
    internal_wlc: u8,
    mode: PPUMode,
    cycles: u8,
    sprite_buffer: Option<u16>,
    current_sprite_palette: Option<bool>,
    sprite: Vec<Sprite>,
    lcd: Vec<Pixel>,
    interrupt: InterruptRef,
}
impl Ppu {
    pub fn new(registers: Wrapper<PPURegisters>, interrupt: InterruptRef) -> Self {
        Ppu {
            interrupt,
            bg_buffer: 0,
            internal_wlc: 0,
            sprite_buffer: None,
            cycles: 0,
            current_sprite_palette: None,
            mode: PPUMode::Oam,
            registers,
            sprite: vec![],
            lcd: vec![],
        }
    }

    fn get_interrupt_stat(&self, source: LCDInterruptSource) -> bool {
        let bit = match source {
            LCDInterruptSource::LyInterrupt => 6,
            LCDInterruptSource::OamInterrupt => 5,
            LCDInterruptSource::VblankInterrupt => 4,
            LCDInterruptSource::HblankInterrupt => 3,
        };
        let value = self.registers.borrow().stat;
        get_bit!(value, bit) == 1
    }

    fn get_lcdc_flag(&self, register: LCDCRegisters) -> bool {
        use LCDCRegisters::*;
        let lcdc = self.registers.borrow().lcdc;
        let bit = match register {
            LCDEnable => 7,
            WindowTileMapArea => 6,
            WindowEnable => 5,
            TileDataArea => 4,
            TileMapArea => 3,
            SpriteSize => 2,
            SpriteEnable => 1,
            BGPriority => 0,
        };
        get_bit!(lcdc, bit) == 1
    }

    fn get_current_ppu_mode(&mut self) -> PPUResult<PPUMode> {
        let mode = self.registers.borrow().stat & 0b11;
        PPUMode::get_mode(mode)
    }

    // TODO: Set PPU Mode
    pub fn oam_search(&mut self) {
        let register = self.registers.borrow();
        let sprite_size = if self.get_lcdc_flag(LCDCRegisters::SpriteSize) {
            16
        } else {
            8
        };
        for i in 0..(register.oam.len() / 4) {
            let start_location = i * 4;
            if self.sprite.len() == 10 {
                return;
            }
            if register.oam[start_location + 1] > 0
                && register.ly + 16 >= register.oam[start_location]
                && register.ly < register.oam[start_location] + sprite_size
            {
                self.sprite
                    .push(Sprite::new(&register.oam[(i * 4)..((i * 4) + 4)]));
            }
        }
    }

    fn change_ppu_mode(&mut self, mode: PPUMode) {
        let value = self.registers.borrow().stat;
        let mut register = self.registers.borrow_mut();
        register.stat = clear_bits!(value, 1) | mode.get_bit_value();
        self.mode = mode;
    }

    // TODO: Set PPU Mode
    fn pixel_fetcher(&mut self) -> PPUResult<()> {
        use LCDCRegisters::*;
        let register = self.registers.borrow();
        let sprite_size = self.get_lcdc_flag(SpriteSize);
        let mut previous_sprite_x = 0;
        let mut pixel: Pixel;

        // Check if the line being fetched is a window line
        if self.get_lcdc_flag(WindowEnable) {
            if register.ly >= register.wy {
                self.internal_wlc += 1;
            }
        }
        for current_x_pos in 0..160 {
            let tile_data_area = if self.get_lcdc_flag(TileDataArea) {
                0x8000
            } else {
                0x8800
            };
            let bg_tile_map_area = if self.get_lcdc_flag(TileMapArea) {
                0x9C00
            } else {
                0x9800
            };
            let window_tile_map_area = if self.get_lcdc_flag(WindowTileMapArea) {
                0x9C00
            } else {
                0x9800
            };
            let tile_data_area = tile_data_area - 0x8000;
            let bg_tile_map_area = bg_tile_map_area - 0x8000;
            let window_tile_map = window_tile_map_area - 0x8000;

            // WINDOW RENDERING
            // This means that we are inside a window
            self.bg_buffer = if register.ly >= register.wy
                && current_x_pos >= register.wx as u16
                && self.get_lcdc_flag(WindowEnable)
            {
                let tile_number_address =
                    (current_x_pos + (32 * self.internal_wlc as u16 / 8)) & 0x3FF;
                let tile_number =
                    register.vram[window_tile_map_area + tile_number_address as usize];
                let low_byte_address =
                    tile_data_area + tile_number as usize + (2 * self.internal_wlc as usize);
                let low_byte = register.vram[low_byte_address];
                let hi_byte = register.vram[low_byte_address + 1];
                Pixel::transfrom_to_line(low_byte, hi_byte)
            } else {
                if self.bg_buffer == 0 {
                    let tile_number_address = (((current_x_pos + (register.scx as u16 / 8))
                        & 0x1F)
                        + (32 * (((register.ly as u16 + register.scy as u16) & 0xFF) / 8)))
                        & 0x3FF;
                    let tile_number =
                        register.vram[bg_tile_map_area + tile_number_address as usize];
                    let low_byte_address = tile_data_area
                        + tile_number as usize
                        + (2 * ((register.ly as usize + register.scy as usize) % 8));
                    let low_byte = register.vram[low_byte_address];
                    let hi_byte = register.vram[low_byte_address + 1];
                    Pixel::transfrom_to_line(low_byte, hi_byte)
                } else {
                    self.bg_buffer
                }
            };
            self.current_sprite_palette = if let Some(sprite) = Sprite::sprite_to_render(
                &self.sprite,
                current_x_pos as u8 + register.scx,
                register.ly,
                sprite_size,
            ) {
                if let Some(prev_sprite_buffer) = self.sprite_buffer {
                    let offset = if previous_sprite_x != 0 {
                        sprite.pos_x - previous_sprite_x
                    } else {
                        0
                    };
                    let line =
                        sprite.get_line(register.ly - sprite.pos_y, sprite_size, &register.vram)?;
                    let line = prev_sprite_buffer | (line.reverse_bits() >> offset).reverse_bits();
                    self.sprite_buffer = Some(line);
                } else {
                    let line =
                        sprite.get_line(register.ly - sprite.pos_y, sprite_size, &register.vram)?;
                    self.sprite_buffer = Some(line);
                }
                previous_sprite_x = sprite.pos_x;
                Some(sprite.palette)
            } else {
                None
            };
            (pixel, self.bg_buffer, self.sprite_buffer) = self.pixel_mixer(current_x_pos as u8)?;
            self.lcd.push(pixel);
        }
        Ok(())
    }

    // TODO: Fix Bacground Pallette
    fn pixel_mixer(&self, pos: u8) -> PPUResult<(Pixel, u16, Option<u16>)> {
        let register = self.registers.borrow();
        //Todo: Fix the offset calculation
        let offset = ((register.scx % 8) + pos) * 2;

        let bg_fifo = self.bg_buffer.reverse_bits() >> offset;
        if let Some(sprite_fifo) = self.sprite_buffer {
            let sprite_fifo = sprite_fifo.reverse_bits() >> offset;
            let bg_obj_priority = self.get_lcdc_flag(LCDCRegisters::BGPriority);
            let sprite_pixel = sprite_fifo & 0b11;
            let bacground_pixel = bg_fifo & 0b11;
            let result_pixel = if (sprite_pixel == 0) || (bg_obj_priority && bacground_pixel > 0) {
                Pixel::new(bacground_pixel as u8, false)?
            } else {
                Pixel::new(sprite_pixel as u8, self.current_sprite_palette.unwrap())?
            };
            let bg_fifo = clear_bits!(bg_fifo, offset).reverse_bits();
            let sprite_fifo = Some(clear_bits!(sprite_fifo, offset).reverse_bits());
            return Ok((result_pixel, bg_fifo, sprite_fifo));
        }
        let result_pixel = Pixel::new((bg_fifo & 0b11) as u8, false)?;

        let bg_fifo = clear_bits!(bg_fifo, offset).reverse_bits();

        return Ok((result_pixel, bg_fifo, None));
    }

    // TODO: Need to do some stuffs
    fn hblank(&mut self) {
        self.cycles = 40;
    }

    // TODO: Need to do some stuffs
    fn vblank(&mut self) {
        self.cycles = 40;
        self.interrupt
            .borrow_mut()
            .add_sources(vec![InterruptSource::Vblank]);
    }

    fn handle_interrupt(&mut self) {
        let registers = self.registers.borrow();
        if self.get_interrupt_stat(LCDInterruptSource::LyInterrupt) {
            if get_bit!(registers.stat, 2) == 1 {
                if registers.ly == registers.lyc {
                    self.interrupt
                        .borrow_mut()
                        .add_sources(vec![InterruptSource::LcdStat])
                }
            } else {
                if registers.ly != registers.lyc {
                    self.interrupt
                        .borrow_mut()
                        .add_sources(vec![InterruptSource::LcdStat])
                }
            }
        }
    }

    pub fn execute(&mut self) -> PPUResult<()> {
        if self.cycles == 0 {
            if self.get_lcdc_flag(LCDCRegisters::LCDEnable) {
                match self.get_current_ppu_mode()? {
                    PPUMode::Oam => {
                        // Clear the previous frame and start over again
                        if self.lcd.len() > 0 {
                            self.lcd = vec![];
                        }
                        self.oam_search();
                        self.change_ppu_mode(PPUMode::Pushing);
                    }
                    PPUMode::Pushing => {
                        self.pixel_fetcher()?;
                        self.change_ppu_mode(PPUMode::Hblank)
                    }
                    PPUMode::Hblank => {
                        self.hblank();
                        // If it is done drawing the screen
                        if self.lcd.len() == 160 * 144 {
                            self.change_ppu_mode(PPUMode::Vblank);
                        } else {
                            self.change_ppu_mode(PPUMode::Oam);
                        }
                    }
                    PPUMode::Vblank => {
                        self.vblank();
                        self.change_ppu_mode(PPUMode::Oam);
                    }
                }
                self.handle_interrupt();
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod ppu_test {

    use super::*;
    // This is test to test the tile struct to see if it correctly gets line data
    #[test]
    fn test_tile_get_line() {
        let vram = [
            0x3C, 0x7E, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x7E, 0x5E, 0x7E, 0x0A, 0x7C, 0x56,
            0x38, 0x7C,
        ];
        let tile = Tile::new(0, &vram);
        let line = tile.get_line(1).map_err(|e| println!("{e}")).unwrap();
        assert_eq!(0b0011000000001100, line);
    }
    #[test]
    #[should_panic]
    fn test_tile_get_line_panic() {
        let vram = [
            0x3C, 0x7E, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x7E, 0x5E, 0x7E, 0x0A, 0x7C, 0x56,
            0x38, 0x7C,
        ];
        let tile = Tile::new(0, &vram);
        let line = tile.get_line(8).map_err(|e| println!("{e}")).unwrap();
        assert_eq!(0b0011000000001100, line);
    }
}
