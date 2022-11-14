use std::fmt::Display;

use crate::{construct_16bit, get_bit, ppu_registers::PPURegisters, InterruptRef, Wrapper};

/// This is the screen size for the Gameboy Screen
const SCREEN_SIZE: usize = 160 * 144;

/// This is representation of what value the pixel is
#[derive(Clone, Copy)]
enum Pixel {
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

/// We have two type of tiles
enum TileType {
    /// This is a normal 8x8 Tile
    Normal,
    /// This is a tall tile that is 8x16
    Tall,
}

pub enum PPUError {}

impl Display for PPUError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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

/// We have 3 types of displays that are super imposed over each other.
enum DisplayType {
    /// The background layer has a z-index of 0<br>
    /// It is 32x32 Tile grid
    Background,
    /// The Window layer has a z-index of 1<br>
    /// It is a 32x32 Tile grid
    Window,
    /// The Sprite layer has a z-index of 2<br>
    /// This is a combination of types that sits on top of everything else
    Sprite,
}

/// This is a representation of a sprite
struct Sprite {
    x: u8,
    y: u8,
    tile_number: u8,
    flags: u8,
}

/// This is a representation that can be used to get the value of a flag
enum SpriteFlags {
    /// This flag is used to determine if a sprite is rendered above the background<br>
    /// When it is not set, the sprite is always rendered above the background<br>
    /// When it is set, the background pixel with a value between 1 - 3(LowOn-Off) would be rendered above the sprite
    BgPriority,
    /// This flag is used to show if the sprite is flipped vertically
    FlipY,
    /// This flag is used to show if the sprite is flipped horizontally
    FlipX,
    /// This flag is used to show if the sprite should use `ObGP0` flag or the `ObGP1` flag
    PalleteNumber,
}

impl SpriteFlags {
    /// This method gets the current state of the flag from the byte
    fn get_flag_state(byte: u8, flag: Self) -> bool {
        use SpriteFlags::*;
        let bit: u8 = match flag {
            BgPriority => 7,
            FlipY => 6,
            FlipX => 5,
            PalleteNumber => 4,
        };
        get_bit!(byte, bit) == 1
    }
}

impl Sprite {
    /// This method returns a new Sprite object from bytes
    fn new(bytes: &[u8]) -> Self {
        if bytes.len() != 4 {
            panic!("Sprite was passed in the wrong number of bytes: {bytes:?}")
        }
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
    register: Wrapper<PPURegisters>,
}

impl PixelFetcher {
    pub fn new(register: Wrapper<PPURegisters>) -> Self {
        PixelFetcher {
            internal_x_pos: 0,
            window_line_counter: 0,
            register,
        }
    }

    fn check_inside_window(&self) -> bool {
        let registers = self.register.borrow();
        self.internal_x_pos >= registers.wx
            && registers.ly >= registers.wy
            && LcdcFlags::get_lcdc_flag_status(registers.lcdc, LcdcFlags::WindowEnable)
    }

    pub fn fetch_tile_no(&mut self) -> u8 {
        let register = self.register.borrow();
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
        // many pixels from the left to know the current tile that should be rendered (I think that
        // internal_x_pos should be divided by 8 but I'm not sure more testing is required)
        let fetcher_x = if self.check_inside_window() {
            // ((register.wx + register.scx) / 8) as usize
            register.wx as usize
        } else {
            (((register.scx / 8) + self.internal_x_pos) & 0x1f) as usize
        };
        // This gets the tile at the start of the viewport from the y-direction(scy/8) and then we
        // add it with `ly/8` in order to get the current tile from the start of the y-direction of
        // the viewport. We then multiply by the value by 32 to get the current index in the vram
        // where the tile is.
        let fetcher_y = if self.check_inside_window() {
            32 * (self.window_line_counter as usize)
        } else {
            32 * (((register.ly + register.scy) & 0xFF) / 8) as usize
        };
        // We and the values with `0x3FF` in order not to go out of bounds of memory
        let offset = (fetcher_x & 0x3FF) + (fetcher_y & 0x3FF);
        let tile_map_area = if self.check_inside_window() {
            window_tile_map_area
        } else {
            bg_tile_map_area
        };
        register.vram[tile_map_area + offset]
    }

    fn fetch_tile_data(&mut self, tile_no: u8) -> u16 {
        let register = self.register.borrow();
        let tile_data_area =
            if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::BgWindowTileDataArea) {
                0x8000
            } else {
                0x8800
            } - 0x8000;
        let offset = if self.check_inside_window() {
            2 * (self.window_line_counter % 8)
        } else {
            2 * ((register.scy + register.ly) % 8)
        };
        let tile_data_low_address = (tile_no + offset) as usize;
        let tile_data_hi_address = (tile_data_low_address + 1) as usize;
        let hi_byte = register.vram[tile_data_area + tile_data_hi_address];
        let lo_byte = register.vram[tile_data_area + tile_data_low_address];
        construct_16bit(hi_byte, lo_byte)
    }
}

/// This is the main struct of the Pixel Processing unit that holds all the data the PPU needs
pub struct Ppu {
    /// This is an array of Pixels that represent a Gameboy Screen
    lcd: [Pixel; SCREEN_SIZE],
    /// This is a reference to the PPU registers
    registers: Wrapper<PPURegisters>,
    /// This is a buffer of sprites that contain buffers that was returned in the last OAM Scan
    sprite_buffer: Vec<Sprite>,
    /// This is the handler that handles all PPU interrupts
    interrupt_handler: InterruptRef,
    /// This is the internal X value that is used during the pixel fetch
    internal_x_pos: u8,
}

impl Ppu {
    /// This method returns a new instance of the Ppu Struct with default values
    pub fn new(registers: Wrapper<PPURegisters>, interrupt_handler: InterruptRef) -> Self {
        Ppu {
            lcd: [Pixel::On; SCREEN_SIZE],
            registers,
            interrupt_handler,
            sprite_buffer: vec![],
            internal_x_pos: 0,
        }
    }

    /// This method scans the oam for sprites that should currently be on the screen<br>
    /// For a sprite to render on the screen, it should have
    /// - It's X position to be greater than 0
    /// - LY + 16 must be greater than or equal to the sprite's Y Position
    /// - LY + 16 must be less than Sprite's Y-Position + Sprite Height
    /// - The ppu's sprite buffer must have less than 10.
    fn oam_scan(&mut self, oam: &[u8]) {
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
    }

    /// This is a method that is meant to fetch the pixels that need to be fetched<br>
    /// We have multiple steps,
    /// - **First Step (Fetch Background/Window Tile Number)**
    ///
    fn pixel_fetcher(&mut self) {
        let register = self.registers.borrow();
        let bg_tile_map_area =
            if LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::BgTileMapArea) {
                0x9C00
            } else {
                0x9800
            } - 0x8000;
        // Getting the Tile number
        // Fetching a background Pixel
        let offset = if !LcdcFlags::get_lcdc_flag_status(register.lcdc, LcdcFlags::WindowEnable) {
            (((register.scx / 8) + self.internal_x_pos) & 0x1f)
                + (32 * (((register.ly + register.scy) & 0xFF) / 8))
        } else {
            0
        };
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::BufReader};

    use crate::{
        cartridge::Cartridge, cpu::Cpu, interrupt::Interrupt, io_registers::IORegisters,
        memory::Memory, wrap_with_wrapper,
    };

    use super::*;
    fn get_dummy_ppu(test_cart: &str) -> Ppu {
        let cartridge = Cartridge::new(test_cart).unwrap();
        let ppu_registers = wrap_with_wrapper(PPURegisters::new());
        let io_registers = wrap_with_wrapper(IORegisters::default());
        let boot_file = File::open("src/roms/boot.gb").unwrap();
        let boot_rom = BufReader::with_capacity(256, boot_file);
        let memory = Memory::new(
            cartridge,
            boot_rom,
            io_registers.clone(),
            ppu_registers.clone(),
        );
        let mem_ref = wrap_with_wrapper(memory);
        let cpu = Cpu::new(mem_ref.clone());
        let cpu_ref = wrap_with_wrapper(cpu);
        let interrupt = Interrupt::new(cpu_ref.clone(), io_registers.clone());
        let interrupt_ref = wrap_with_wrapper(interrupt);
        io_registers
            .borrow_mut()
            .add_interrupt_handler(interrupt_ref.clone());
        let ppu = Ppu::new(ppu_registers.clone(), interrupt_ref.clone());
        ppu
    }
    #[test]
    fn test_oam_scan() {
        let mut ppu = get_dummy_ppu("src/roms/cpu_instrs.gb");
        let oam = [2, 16, 0x12, 0b11110000];
        ppu.oam_scan(&oam);
        assert_eq!(ppu.sprite_buffer.len(), 1);
    }
}
