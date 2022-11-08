use crate::{get_bit, ppu_registers::PPURegisters, Wrapper};

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

/// This is a representation that can be used to get the value of an LCDC flag
enum LcdcFlags {
    /// This is a flag that shows if the lcd is enabled or disabled<br>
    LcdEnable,
    /// This is a flag that determines where the window tile map should be gotten from<br>
    /// When the flag is set, the tile map area is `0x9C00-0x9FFF`<br>
    /// When the flag is not set, the tile map area is `0x9800-0x9BFF`
    WindowTileMapArea,
    /// This is a flag that determines if a window is enabled or disabled
    WindowEnable,
    /// This is a flag that determines where the bg/window tile map should be gotten from<br>
    BgWindowTileDataArea,
    BgTileMapArea,
    SpriteSize,
    SpriteEnable,
    BgWindowPriority
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
    fn new(bytes: [u8; 4]) -> Self {
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

/// This is the main struct of the Pixel Processing unit that holds all the data the PPU needs
struct Ppu {
    /// This is an array of Pixels that represent a Gameboy Screen
    lcd: [Pixel; SCREEN_SIZE],
    /// This is a reference to the PPU registers
    registers: Wrapper<PPURegisters>,
    /// This is a buffer of sprites that contain buffers that was returned in the last OAM Scan
    sprite_buffer: Vec<Sprite>,
}

impl Ppu {
    /// This method returns a new instance of the Ppu Struct with default values
    fn new(registers: Wrapper<PPURegisters>) -> Self {
        Ppu {
            lcd: [Pixel::On; SCREEN_SIZE],
            registers,
            sprite_buffer: vec![],
        }
    }

    /// This method scans the oam for sprites that should currently be on the screen<br>
    /// For a sprite to render on the screen, it should have
    /// - It's X position to be greater than 0
    /// - LY + 16 must be greater than or equal to the sprite's Y Position
    /// - LY + 16 must be less than Sprite's Y-Position + Sprite Height
    /// - The ppu's sprite buffer must have less than 10.
    fn oam_scan(&mut self) {
        let register = self.registers.borrow();
        for i in 0..40 {
            let start_index = i * 4;
            let sprite_x = start_index;
            let sprite_y = start_index + 1;
            if sprite_x > 0 && register.ly + 16 >= sprite_y && register.ly + 16 < sprite_y + 
        }
    }
}
