use std::fmt::Display;

use crate::{
    clear_bits, get_bit, get_bits,
    memory::{Memory, MemoryError},
    ppu_registers::PPURegisters,
    Bus, EmulatorError, Wrapper,
};

type PPUResult<T> = Result<T, PPUError>;

/// This is the screen size for the Gameboy Screen
pub const SCREEN_SIZE: usize = 160 * 144;

/// This is representation of what value the pixel is
#[derive(Clone, Copy)]
pub enum Pixel {
    /// This is a black color<br>
    /// It 2-bit value is `0b11`
    Off,
    /// This is a dark grey color<br>
    /// It 2-bit value is `0b10`
    LowOn,
    /// This is a light grey color<br>
    /// It's 2-bit value is `0b01`
    HiOn,
    /// This is a White color<br>
    /// It's 2-bit value is `0b00`
    On,
}

impl From<u8> for Pixel {
    fn from(value: u8) -> Self {
        use Pixel::*;
        match value {
            0b00 => On,
            0b01 => HiOn,
            0b10 => LowOn,
            0b11 => Off,
            _ => unreachable!(),
        }
    }
}

impl Display for Pixel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pixel::Off => "\u{2588}",
            Pixel::LowOn => "\u{2593}",
            Pixel::HiOn => "\u{2592}",
            Pixel::On => "\u{2591}",
        }
        .fmt(f)
    }
}

impl Pixel {
    #[inline]
    fn from_palette(value: u16, palette: u8) -> Self {
        let pixel = get_bits!(palette >> (value * 2), 2);
        Pixel::from(pixel)
    }
}

pub enum PPUError {
    InvalidPixel(u8),
    MemoryError(MemoryError),
}

impl From<PPUError> for EmulatorError {
    fn from(error: PPUError) -> Self {
        Self::PPUError(error)
    }
}

impl Display for PPUError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PPUError::InvalidPixel(pixel) => format!("Invalid pixel `{pixel:08b}` was given"),
            PPUError::MemoryError(error) => {
                format!("PPU threw a error when trying to access Memeory: `{error}`")
            }
        }
        .fmt(f)
    }
}

impl From<MemoryError> for PPUError {
    fn from(error: MemoryError) -> Self {
        Self::MemoryError(error)
    }
}

/// This is a representation that can be used to get the value of an LCDC flag
pub enum LcdcFlags {
    /// This is a flag that shows if the lcd is enabled or disabled<br>
    LcdEnable,
    /// This is a flag that determines where the window tile map should be gotten from<br>
    /// When the flag is set, the tile map area is `0x9C00-0x9FFF`<br>
    /// When the flag is not set, the tile map area is `0x9800-0x9BFF`
    WindowTileMapArea,
    /// This is a flag that determines if a window is enabled or disabled
    WindowEnable,
    /// This is a flag that determines where the bg/window tile data area should be gotten from<br>
    /// When the flag is set, the tile Data Area is`0x8000-0x8FFF`<br>
    /// When the flag is not set, the Tile Data Area is `0x8800-0x97FF`
    BgWindowTileDataArea,
    /// This flag that determines where the bg/window Tile Map Area should be gotten from<br>
    /// When the flag is set, the Tile Map Area is `0x9C00-0x9FFF`<br>
    /// When the flag is not set, the Tile Map Area is `0x9800-0x9BFF`
    BgTileMapArea,
    /// This flag that determines the size of sprites globally<br>
    /// When the flag is set, the size is`8x16`<br>
    /// When the flag is not set, the size is `8x8`
    SpriteSize,
    /// This flag that determines if sprites should be drawn or not
    SpriteEnable,
    /// This flag that determine if a sprite should draw on top of the background<br>
    /// When the flag is set, the background colors (1-3) draw on top of the sprite<br>
    /// When the flag is not set, the Sprite draws on top of the background
    BgWindowPriority,
}

impl LcdcFlags {
    #[inline]
    pub fn get_lcdc_flag_status(lcdc: u8, flag: LcdcFlags) -> bool {
        let bit: u8 = match flag {
            LcdcFlags::LcdEnable => 7,
            LcdcFlags::WindowTileMapArea => 6,
            LcdcFlags::WindowEnable => 5,
            LcdcFlags::BgWindowTileDataArea => 4,
            LcdcFlags::BgTileMapArea => 3,
            LcdcFlags::SpriteSize => 2,
            LcdcFlags::SpriteEnable => 1,
            LcdcFlags::BgWindowPriority => 0,
        };
        get_bit!(lcdc, bit) == 1
    }
}

// /// We have 3 types of displays that are super imposed over each other.
// enum DisplayType {
//     /// The background layer has a z-index of 0<br>
//     /// It is 32x32 Tile grid
//     Background,
//     /// The Window layer has a z-index of 1<br>
//     /// It is a 32x32 Tile grid
//     Window,
//     /// The Sprite layer has a z-index of 2<br>
//     /// This is a combination of types that sits on top of everything else
//     Sprite,
// }

/// This is a representation of a sprite
struct Sprite {
    pub x: u8,
    pub y: u8,
    pub tile_number: u8,
    flags: u8,
}

/// This is a representation that can be used to get the value of a flag
enum SpriteFlags {
    /// This flag is used to show if the sprite should use `ObGP0` flag or the `ObGP1` flag
    PalleteNumber,
    /// This flag is used to show if the sprite is flipped horizontally
    FlipX,
    /// This flag is used to show if the sprite is flipped vertically
    FlipY,
    /// This flag is used to determine if a sprite is rendered above the background<br>
    /// When it is not set, the sprite is always rendered above the background<br>
    /// When it is set, the background pixel with a value between 1 - 3(LowOn-Off) would be rendered above the sprite
    BgPriority,
}

impl Sprite {
    /// This method returns a new Sprite object from bytes
    fn new(bytes: &[u8]) -> Self {
        Sprite {
            x: bytes[0],
            y: bytes[1],
            tile_number: bytes[2],
            flags: bytes[3],
        }
    }
    /// This method gets the current state of the flag from the byte
    fn get_flag_state(&self, flag: SpriteFlags) -> bool {
        use SpriteFlags::*;
        let bit: u8 = match flag {
            BgPriority => 7,
            FlipY => 6,
            FlipX => 5,
            PalleteNumber => 4,
        };
        get_bit!(self.flags, bit) == 1
    }
}

pub struct PixelFetcher {
    internal_x_pos: u8,
    window_line_counter: u8,
    /// This is a boolean value that represents if a window pixel was drawn on that line
    inside_window: bool,
    register: Wrapper<PPURegisters>,
}

impl PixelFetcher {
    pub fn new(register: Wrapper<PPURegisters>) -> Self {
        PixelFetcher {
            internal_x_pos: 0,
            window_line_counter: 0,
            inside_window: false,
            register,
        }
    }

    pub fn reset(&mut self) {
        self.internal_x_pos = 0;
        self.window_line_counter = 0;
    }

    fn get_sprite_buffer(&self, buffer: &[Sprite]) -> Option<(u16, u8, bool)> {
        let register = self.register.borrow();
        let sprite_height = if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::SpriteSize)
        {
            16
        } else {
            8
        };
        let sprite = buffer
            .iter()
            .filter(|&sprite| {
                self.internal_x_pos >= sprite.x
                    && self.internal_x_pos < sprite.x + 8
                    && register.ly + 16 >= sprite.y
                    && register.ly + 16 < sprite.y + sprite_height
                    && LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::SpriteEnable)
            })
            .min_by(|&x, &y| x.x.cmp(&y.x));
        if let Some(sprite) = sprite {
            let pallete = if sprite.get_flag_state(SpriteFlags::PalleteNumber) {
                self.register.borrow().pallete_1
            } else {
                self.register.borrow().pallete_0
            };

            let priority = sprite.get_flag_state(SpriteFlags::BgPriority);
            // This gets the current tiles for the current line of the sprite that is being
            // rendered
            let mut tile_no = sprite.tile_number as usize + ((register.ly - sprite.y) as usize * 2);
            // TODO: Verify logic for Flipping of sprites
            if sprite.get_flag_state(SpriteFlags::FlipY) {
                tile_no =
                    sprite.tile_number as usize + (16 - (tile_no - sprite.tile_number as usize))
            }
            let tile_data_area = 0x8000;
            let low_byte_address = tile_data_area + tile_no;
            let low_byte = register.vram[low_byte_address];
            let hi_byte = register.vram[low_byte_address + 1];
            let final_line = transform_to_line(hi_byte, low_byte);
            let mut line = if sprite.get_flag_state(SpriteFlags::FlipX) {
                final_line.reverse_bits()
            } else {
                final_line
            };
            // Weird fix for a potential bug
            line = if sprite.x == self.internal_x_pos {
                line
            } else {
                get_bits!(line, (self.internal_x_pos - sprite.x) * 2).reverse_bits()
            };

            return Some((line, pallete, priority));
        }
        None
    }

    fn check_inside_window(&self) -> bool {
        let registers = self.register.borrow();
        self.internal_x_pos >= registers.wx
            && registers.ly >= registers.wy
            && LcdcFlags::get_lcdc_flag_status(registers.lcdc, LcdcFlags::WindowEnable)
    }

    /// This function fetches the tile number for the current background tile.
    pub fn fetch_tile_no(&mut self) -> u8 {
        let register = self.register.borrow();
        self.inside_window = self.check_inside_window()
            & LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::BgWindowPriority);
        let bg_tile_map_area =
            if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::BgTileMapArea) {
                0x9C00
            } else {
                0x9800
            } - 0x8000;
        let window_tile_map_area =
            if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::WindowTileMapArea) {
                0x9C00
            } else {
                0x9800
            } - 0x8000;

        // This gets the tile at the start of the viewport from the x-direction(scx /8) and then we add it by how
        // many pixels from the left to know the current tile that should be rendered
        let fetcher_x = if self.inside_window {
            register.wx as usize
        } else {
            // We cast `scx` and `internal_x_pos` to u16 so that the addition would not overflow
            ((register.scx as u16 + self.internal_x_pos as u16) & 0x1F) as usize
        };
        // This gets the tile at the start of the viewport from the y-direction(scy/8) and then we
        // add it with `ly/8` in order to get the current tile from the start of the y-direction of
        // the viewport. We then multiply by the value by 32 to get the current index in the vram
        // where the tile is.
        let fetcher_y = if self.inside_window {
            32 * (self.window_line_counter as usize)
        } else {
            // We cast `ly` and `scy` to u16 so that the addition would not overflow
            32 * (((register.ly as u16 + register.scy as u16) & 0xFF) / 8) as usize
        };
        // We and the values with `0x3FF` in order not to go out of bounds of memory
        let offset = (fetcher_x & 0x3FF) + (fetcher_y & 0x3FF);
        let tile_map_area = if self.inside_window {
            window_tile_map_area
        } else {
            bg_tile_map_area
        };
        register.vram[tile_map_area + offset]
    }

    /// This function fetches the tile data (both the low and high byte for the current line of the
    /// background tile that is being rendered) for the current background tile
    pub fn fetch_tile_data(&mut self, tile_no: u8) -> u16 {
        let register = self.register.borrow();
        let tile_data_area =
            if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::BgWindowTileDataArea) {
                0x8000
            } else {
                0x8800
            } - 0x8000;
        let offset = if self.inside_window {
            2 * (self.window_line_counter % 8)
        } else {
            2 * ((register.scy + register.ly) % 8)
        };
        let tile_data_low_address = (tile_no + offset) as usize;
        let tile_data_hi_address = (tile_data_low_address + 1) as usize;
        let hi_byte = register.vram[tile_data_area + tile_data_hi_address];
        let low_byte = register.vram[tile_data_area + tile_data_low_address];
        // If this the BGWindowPriority is there then draw the background/window pixels else just draw white
        if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::BgWindowPriority) {
            transform_to_line(hi_byte, low_byte)
        } else {
            0
        }
    }
}

/// This function takes 2 bytes and encodes them in the gameboy's 2bpp format
fn transform_to_line(hi_byte: u8, low_byte: u8) -> u16 {
    let mut result = 0;
    for bit_pos in 0..8 {
        result |= (((get_bit!(hi_byte, bit_pos) << 1) | (get_bit!(low_byte, bit_pos))) as u16)
            << bit_pos * 2;
    }
    result
}

#[derive(Clone, Copy)]
pub enum PPUMode {
    /// The PPU enters this mode once it is done processing a line (internal-fetcher-x-pos = 160)
    Hblank,
    /// This PPU enters this mode once it is done filling up all the Pixels on the screen with
    /// color. It tries to render 10 lines that don't exist
    Vblank,
    /// The PPU enters this mode when it is looking for sprites that are meant to be on the
    /// current line(specified LY) that the PPU is rendering
    OamScan,
    /// This is where the PPU does majority of its work. In this mode, the PPU fetches pixel from
    /// the VRAM Tile Map, uses the tile Map to get the correct tiles from the VRAM Tile Data,
    /// checks if a Sprite should be in view, fetches the tile data and then mixes the pixels
    /// together
    Pushing,
}

impl From<u8> for PPUMode {
    fn from(mode: u8) -> Self {
        match mode {
            0 => Self::Hblank,
            1 => Self::Vblank,
            2 => Self::OamScan,
            3 => Self::Pushing,
            _ => unreachable!(),
        }
    }
}

impl Display for PPUMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PPUMode::Hblank => "Hblank",
            PPUMode::Vblank => "Vblank",
            PPUMode::OamScan => "OamScan",
            PPUMode::Pushing => "Pushing",
        }
        .fmt(f)
    }
}

struct StatInterruptHandler {
    register: Wrapper<PPURegisters>,
    memory: Wrapper<Memory>,
    sources: [bool; 4],
}

impl StatInterruptHandler {
    fn new(register: Wrapper<PPURegisters>, memory: Wrapper<Memory>) -> Self {
        StatInterruptHandler {
            register,
            memory,
            sources: [false; 4],
        }
    }

    pub fn check(&mut self) {
        let stat = self.register.borrow().stat;
        let current_mode = get_bits!(stat, 2);
        for source in 3..7 {
            let index = source - 3;
            if get_bit!(stat, source) == 1 {
                let event_occured = if source == 6 {
                    get_bit!(stat, 2) == 1
                } else {
                    current_mode == index
                };
                if event_occured && !self.sources[index as usize] {
                    // Throw the interrupt
                    self.memory.borrow_mut().write(0xFF0F, 0b10).unwrap();
                    self.sources[index as usize] = true;
                    break;
                }
                self.sources[index as usize] = true;
            } else {
                self.sources[index as usize] = false;
            }
        }
    }
}

//TODO: Add Cycles to the PPU Struct
/// This is the main struct of the Pixel Processing unit that holds all the data the PPU needs
pub struct Ppu {
    /// This is an array of Pixels that represent a Gameboy Screen
    lcd: [Pixel; SCREEN_SIZE],
    /// This is a reference to the PPU registers
    registers: Wrapper<PPURegisters>,
    /// This is a buffer of sprites that contain buffers that was returned in the last OAM Scan
    sprite_buffer: Vec<Sprite>,
    /// This is a reference to the System's Memory
    memory: Wrapper<Memory>,
    /// This is the Interrupt handler that handles LCDStat interrupts
    stat_interrupt_handler: StatInterruptHandler,
    /// This is the current mode that the PPU is in
    pub mode: PPUMode,
    /// This is the pixel fetcher that fetches background/window tiles
    pixel_fetcher: PixelFetcher,
    /// This is the background FIFO that holds the background Pixels
    background_fifo: u16,
    /// This is the sprite FIFO that holds the Sprite Pixels and if it is using `0BP0` or `0BP1`
    sprite_fifo: Option<(u16, u8, bool)>,
    /// This is a counter that represents how many cycles of work the PPU has done
    cycles: u16,
}

impl Ppu {
    /// This method returns a new instance of the Ppu Struct with default values
    pub fn new(registers: Wrapper<PPURegisters>, memory: Wrapper<Memory>) -> Self {
        let stat_interrupt_handler = StatInterruptHandler::new(registers.clone(), memory.clone());
        let pixel_fetcher = PixelFetcher::new(registers.clone());
        Ppu {
            cycles: 0,
            lcd: [Pixel::On; SCREEN_SIZE],
            registers,
            stat_interrupt_handler,
            memory,
            sprite_buffer: vec![],
            pixel_fetcher,
            background_fifo: 0,
            sprite_fifo: None,
            mode: PPUMode::OamScan,
        }
    }
    pub fn is_ppu_enabled(&self) -> bool {
        LcdcFlags::get_lcdc_flag_status(self.registers.borrow().lcdc, LcdcFlags::LcdEnable)
    }

    /// This is a function that changes the PPU's mode and updates the access of the `PPURegister`
    fn set_mode(&mut self, mode: PPUMode) -> PPUResult<()> {
        // Sets the read and write access of the vram and oam depending on the previous mode
        match self.mode {
            PPUMode::Hblank | PPUMode::Vblank => {
                let mut registers = self.registers.borrow_mut();
                registers.rw_access_oam = true;
                registers.rw_access_vram = true;
            }
            PPUMode::OamScan => {
                let mut registers = self.registers.borrow_mut();
                registers.rw_access_oam = false;
                registers.rw_access_vram = true;
            }
            PPUMode::Pushing => {
                let mut registers = self.registers.borrow_mut();
                registers.rw_access_oam = false;
                registers.rw_access_vram = false;
            }
        }
        self.mode = mode;
        let stat = self.registers.borrow().stat;
        self.memory
            .borrow_mut()
            .write(0xFF41, clear_bits!(stat, 1) | mode as u8)?;
        Ok(())
    }

    pub fn get_lcd(&self) -> &[Pixel] {
        &self.lcd
    }

    /// This method scans the oam for sprites that should currently be on the screen<br>
    /// For a sprite to render on the screen, it should have
    /// - It's X position to be greater than 0
    /// - LY + 16 must be greater than or equal to the sprite's Y Position
    /// - LY + 16 must be less than Sprite's Y-Position + Sprite Height
    /// - The ppu's sprite buffer must have less than 10.
    // TODO: Add the correct number of cycles
    fn oam_scan(&mut self, oam: &[u8]) {
        // Reset the Sprite Buffer
        self.sprite_buffer = Vec::new();
        let ly = self.registers.borrow().ly;
        let sprite_height =
            if LcdcFlags::get_lcdc_flag_status(self.registers.borrow().lcdc, LcdcFlags::SpriteSize)
            {
                16
            } else {
                8
            };
        for i in 0..(oam.len() / 4) {
            let start_index = i * 4;
            let sprite_x = oam[start_index];
            let sprite_y = oam[start_index + 1];
            if sprite_x > 0
                && ly + 16 >= sprite_y
                && ly + 16 < sprite_y + sprite_height
                && self.sprite_buffer.len() < 10
            {
                let sprite = Sprite::new(&oam[start_index..=(start_index + 3)]);
                self.sprite_buffer.push(sprite);
            }
        }
        self.cycles += 20;
    }

    /// This is a method that is meant to fetch the pixels that need to be fetched<br>
    /// We have multiple steps,
    /// - **First Step (Fetch Background/Window Tile Number)**: In this step, we use the value of
    /// the `SCX`, `LY` registers with an internal fetcher x position to know which tile number of
    /// the background map that should be fetched.
    /// - **Second Step(Fetch Tile Data Low)**: In this step, We use the tile number that was
    /// fetched in the previous step to figure out where in VRAM the next pixels should be loaded
    /// from. We use the `LY` and `SCX` registers to calculate which byte from the tile no that
    /// should the low part of the pixel data come from.
    /// - **Thrid Step (Fetch Tile Data High)**: In this step, we just fetch the next byte after
    /// the byte that was gotten last step.
    // TODO: Add the correct number of cycles
    fn pixel_fetcher(&mut self) {
        let tile_no = self.pixel_fetcher.fetch_tile_no();
        self.background_fifo = self.pixel_fetcher.fetch_tile_data(tile_no);
        self.sprite_fifo = self.pixel_fetcher.get_sprite_buffer(&self.sprite_buffer);
    }

    /// This is the pixel FIFO. This function determines which buffer a pixel would be pushed out onto the LCD
    // TODO: Add the correct number of cycles
    fn pixel_fifo(&mut self) -> Pixel {
        let current_offset = (self.pixel_fetcher.internal_x_pos % 8) * 2;
        let unpushed_bacground_bits = self.background_fifo.reverse_bits() >> current_offset;
        let background_palette = self.registers.borrow().bgp;
        let (pixel_bit, palette) = if let Some((sprite_fifo, palette, priority)) = &self.sprite_fifo
        {
            let palette = *palette;
            let unpushed_sprite_bits = sprite_fifo.reverse_bits() >> current_offset;
            let sprite_pixel = get_bits!(unpushed_sprite_bits, 2);
            if *priority
                && LcdcFlags::get_lcdc_flag_status(
                    self.registers.borrow().lcdc,
                    LcdcFlags::BgWindowPriority,
                )
            {
                if sprite_pixel != 0 {
                    (sprite_pixel, palette)
                } else {
                    (get_bits!(unpushed_bacground_bits, 2), background_palette)
                }
            } else {
                (sprite_pixel, palette)
            }
        } else {
            (get_bits!(unpushed_bacground_bits, 2), background_palette)
        };
        self.pixel_fetcher.internal_x_pos += 1;
        Pixel::from_palette(pixel_bit, palette)
    }

    /// This function represents all the functions that need to be performed during
    /// PPUMode::Pushing
    // TODO: Add the correct number of cycles
    fn push_pixels(&mut self) {
        // Since the pixel fetcher fetches 8 pixels we want to push out those pixels first
        // and then fetch the next one so that's why we are checking if `internal_x_pos % 8 == 0`
        if self.pixel_fetcher.internal_x_pos % 8 == 0 {
            self.pixel_fetcher();
            // println!("Filling up the fifos");
        }
        // println!(
        //     "Pushing pixels with internal_x_pos of `{}` and current line of `{}`",
        //     self.pixel_fetcher.internal_x_pos,
        //     self.registers.borrow().ly
        // );

        let new_pixel = self.pixel_fifo();
        let lcd_index = (self.registers.borrow().ly as usize * 160)
            + self.pixel_fetcher.internal_x_pos as usize;
        self.lcd[lcd_index] = new_pixel;
    }

    // TODO: Add the correct number of cycles
    fn hblank(&mut self) {
        let mut registers = self.registers.borrow_mut();
        // When it's done drawing a line that has window it would increase the internal window line counter
        if self.pixel_fetcher.inside_window {
            self.pixel_fetcher.window_line_counter += 1;
            self.pixel_fetcher.inside_window = false;
        }
        self.pixel_fetcher.internal_x_pos = 0;
        registers.ly += 1;
        self.cycles += 51;
    }
    // TODO: Add the correct number of cycles
    fn vblank(&mut self) {
        let mut registers = self.registers.borrow_mut();
        if registers.ly == 154 {
            registers.ly = 0;
            self.pixel_fetcher.reset();
        }
        self.memory.borrow_mut().write(0xFF0F, 0b1).unwrap();
        self.cycles += 153;
    }

    /// This is the function runs the PPU
    pub fn execute(&mut self) -> PPUResult<()> {
        if LcdcFlags::get_lcdc_flag_status(self.registers.borrow().lcdc, LcdcFlags::LcdEnable) {
            if self.cycles == 0 {
                self.stat_interrupt_handler.check();
                let new_mode = match self.mode {
                    PPUMode::Hblank => {
                        self.hblank();
                        // This checks if it is done rendering the entire screen and then sets the mode to
                        // Vblank. If it is not done it sets it back to OamScan
                        if self.registers.borrow().ly > 143 {
                            PPUMode::Vblank
                        } else {
                            PPUMode::OamScan
                        }
                    }
                    PPUMode::Vblank => {
                        self.vblank();
                        if self.registers.borrow().ly == 0 {
                            PPUMode::OamScan
                        } else {
                            self.mode
                        }
                    }
                    PPUMode::OamScan => {
                        let oam = self.registers.borrow().oam;
                        self.oam_scan(&oam);
                        PPUMode::Pushing
                    }
                    PPUMode::Pushing => {
                        self.push_pixels();
                        // Checks if the we are at the end of a scanline and sets the PPUMode to Hblank
                        if self.pixel_fetcher.internal_x_pos == 159 {
                            PPUMode::Hblank
                        } else {
                            self.mode
                        }
                    }
                };
                // println!(
                //     "I am running in {} with {} cycles remaining",
                //     self.mode, self.cycles
                // );
                self.set_mode(new_mode)?;
            } else {
                self.cycles -= 1;
            }
        } else {
            // println!("LCD is not enabled so we can't draw anything")
        }

        Ok(())
    }
}
