use crate::{get_bit, memory::Memory, ppu_registers::PPURegisters, Wrapper};

// #[derive(Clone, Copy)]
enum Color {
    White,
    LightGrey,
    DarkGrey,
    Black,
    None,
}

// impl Colors {
//     pub fn get_color(bit: u8, sprite: bool) -> Self {
//         use Colors::*;
//         match bit {
//             0b00 => {
//                 if sprite {
//                     Transparent
//                 } else {
//                     Black
//                 }
//             }
//             0b01 => LightGrey,
//             0b10 => DarkGrey,
//             0b11 => White,
//             _ => None,
//         }
//     }
// }


// #[derive(Default, Clone, Copy)]
// struct Tile {
//     tile_data: [u8; 16],
//     sprite: bool,
// }

// impl Tile {
//     pub fn get_line_color(&self, line: u8) -> Vec<Colors> {
//         let mut result = vec![];
//         if line > 7 {
//             panic!("Invalid line `{}` was requested from a tile", line);
//         } else {
//             let first_byte = self.tile_data[line as usize * 2];
//             let second_byte = self.tile_data[(line as usize * 2) + 1];
//             for i in 7..=0 {
//                 let color_bit = get_bit(second_byte, i) << 1 | get_bit(first_byte, i);
//                 result.push(Colors::get_color(color_bit, self.sprite));
//             }
//         }
//         result
//     }
//     pub fn construct_image(&self) -> Vec<Colors> {
//         let mut result = vec![];
//         for i in 0..(self.tile_data.len() / 2) {
//             let first_byte = self.tile_data[i];
//             let second_byte = self.tile_data[i + 1];
//             for i in 7..=0 {
//                 let color_bit = get_bit(second_byte, i) << 1 | get_bit(first_byte, i);
//                 result.push(Colors::get_color(color_bit, self.sprite));
//             }
//         }
//         result
//     }
// }

// struct Background {
//     tiles: [Tile; 1024],
// }

// impl Background {
//     pub fn new() -> Self {
//         Background {
//             tiles: [Tile::default(); 1024],
//         }
//     }
//     pub fn get_viewport(&self, scx: u8, scy: u8) -> [Tile; 20 * 18] {
//         let (starting_x, starting_y) = (scx as usize / 8, scy as usize / 8);
//         let mut result_tiles = [Tile::default(); 360];
//         for row in 0..18 {
//             for col in 0..20 {
//                 let (pos_x, pos_y) = (starting_x + col, starting_y + row);
//                 result_tiles[(row * 20) + col] = self.tiles[((pos_y % 18) * 20) + (pos_x % 20)];
//             }
//         }
//         result_tiles
//     }
// }

// struct Window {
//     wx: u8,
//     wy: u8,
//     tile_data: [Tile; 1024],
// }

// impl Window {
//     pub fn new() -> Self {
//         Window {
//             wx: 0,
//             wy: 0,
//             tile_data: [Tile::default(); 1024],
//         }
//     }
// }
enum PixelSource {
    Sprite,
    Window,
    Background,
    None
}

//? Made this an enum incase I want to also emulate CGB games
enum Palette {
    Pallete0,
    Palette1,
    None
}

struct Pixel {
    color: Color,
    source: PixelSource,
    palette: Palette
}

impl Pixel {
    pub fn new() -> Self {
        Pixel { color: Color::None, source: PixelSource::None, palette: Palette::None }
    }

    pub fn get_line(first_byte: u8, second_byte: u8) -> u16 {
        let mut result = 0;
        for bit_pos in 0..8 {
            result |= (((get_bit(second_byte, bit_pos) << 1) | get_bit(first_byte, bit_pos)) as u16) << bit_pos * 2;
        }
        result
    }

}



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
    fn new(oam_data: &[u8]) -> Self {
        let pos_y = oam_data[0];
        let pos_x = oam_data[1];
        let tile_number = oam_data[2];
        let bg_priority = get_bit(oam_data[3], 7) == 1;
        let flip_y = get_bit(oam_data[3], 6) == 1;
        let flip_x = get_bit(oam_data[3], 5) == 1;
        let palette = get_bit(oam_data[3], 4) == 1;
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
}

enum LCDCRegisters {
    LCDEnable,
    WindowTileMapArea,
    WindowEnable,
    TileDataArea,
    TileMapArea,
    SpriteSize,
    SpriteEnable,
    BGPriority,
}

pub struct Ppu {
    registers: Wrapper<PPURegisters>,
    pixel_fifo: Vec<Pixel>,
    sprite: Vec<Sprite>,
}
impl Ppu {
    pub fn new(registers: Wrapper<PPURegisters>) -> Self {
        Ppu {
            pixel_fifo: Vec::with_capacity(16),
            registers,
            sprite: vec![],
        }
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
        get_bit(lcdc, bit) == 1
    }

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

    


}
