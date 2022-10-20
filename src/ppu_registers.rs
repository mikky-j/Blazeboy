use crate::Bus;

pub struct PPURegisters {
    pub lcdc: u8,
    pub stat: u8,
    pub scx: u8,
    pub scy: u8,
    pub ly: u8,
    pub lyc: u8,
    pub dma: u8,
    pub bgp: u8,
    pub pallete_0: u8,
    pub pallete_1: u8,
    pub wx: u8,
    pub wy: u8,
    pub vram: [u8; 0x2000],
    pub oam: [u8; 0xa0],
}

impl PPURegisters {
    pub fn new() -> Self {
        PPURegisters {
            lcdc: 0,
            stat: 0,
            scx: 0,
            scy: 0,
            ly: 0,
            lyc: 0,
            dma: 0xFF,
            bgp: 0,
            pallete_0: 0,
            pallete_1: 0,
            wx: 0,
            wy: 0,
            vram: [0; 0x2000],
            oam: [0; 0xa0],
        }
    }
}

impl Bus for PPURegisters {
    fn read(&mut self, address: u16) -> Result<u8, crate::memory::MemoryError> {
        let value = match address {
            0x8000..=0xa000 => {
                let offset = address - 0x8000;
                self.vram[offset as usize]
            }
            0xFE00..=0xFE9F => {
                let offset = address - 0xFFE0;
                self.oam[offset as usize]
            }
            0xFF40 => self.lcdc,
            0xFF41 => self.stat,
            0xFF42 => self.scx,
            0xFF43 => self.scy,
            0xFF44 => self.ly,
            0xFF45 => self.lyc,
            0xFF46 => self.dma,
            0xFF47 => self.bgp,
            0xFF48 => self.pallete_0,
            0xFF49 => self.pallete_1,
            0xFF4A => self.wx,
            0xFF4B => self.wy,
            _ => return Err(crate::MemoryError::InvalidRead(address)),
        };
        Ok(value)
    }

    fn write(&mut self, address: u16, value: u8) -> Result<(), crate::memory::MemoryError> {
        match address {
            0x8000..=0xa000 => {
                let offset = address - 0x8000;
                self.vram[offset as usize] = value;
            }
            0xFE00..=0xFE9F => {
                let offset = address - 0xFFE0;
                self.oam[offset as usize] = value;
            }
            0xFF40 => self.lcdc = value,
            0xFF41 => self.stat = value,
            0xFF42 => self.scx = value,
            0xFF43 => self.scy = value,
            0xFF44 => self.ly = value,
            0xFF45 => self.lyc = value,
            0xFF46 => self.dma = value,
            0xFF47 => self.bgp = value,
            0xFF48 => self.pallete_0 = value,
            0xFF49 => self.pallete_1 = value,
            0xFF4A => self.wx = value,
            0xFF4B => self.wy = value,
            _ => return Err(crate::MemoryError::InvalidRead(address)),
        }
        Ok(())
    }
}
