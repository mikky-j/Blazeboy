use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::Color,
    rect::Rect,
    render::{Texture, TextureCreator, TextureQuery},
    ttf::Font,
    video::WindowContext,
};

use crate::{ppu::Pixel, Emulator, SCREEN_SIZE};
const SCREEN_WIDTH: u32 = 600;
const SCREEN_HEIGHT: u32 = 600;
const RECT_SIZE: u32 = 2;

impl Pixel {
    pub fn get_color(&self) -> Color {
        match self {
            Pixel::Off => Color::BLACK,
            Pixel::LowOn => Color::RGBA(98, 98, 98, 255),
            Pixel::HiOn => Color::GRAY,
            Pixel::On => Color::WHITE,
        }
    }
}

pub fn get_font_texture<'a>(
    texture_creator: &'a TextureCreator<WindowContext>,
    text: &str,
    font: &Font,
) -> Result<Texture<'a>, String> {
    let surface = font
        .render(&text)
        .blended(Color::RGBA(255, 0, 0, 255))
        .map_err(|e| e.to_string())?;
    let texture = texture_creator
        .create_texture_from_surface(surface)
        .map_err(|e| e.to_string())?;
    Ok(texture)
}

pub fn run(font_path: &str, mut emulator: Emulator) -> Result<(), String> {
    let sdl_context = sdl2::init()?;
    let video_subsys = sdl_context.video()?;
    let ttf_context = sdl2::ttf::init().map_err(|e| e.to_string())?;

    let window = video_subsys
        .window("SDL2_TTF Example", SCREEN_WIDTH, SCREEN_HEIGHT)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window.into_canvas().build().map_err(|e| e.to_string())?;
    let texture_creator = canvas.texture_creator();

    let lcd_rects = (0..SCREEN_SIZE)
        .map(|index| {
            Rect::new(
                ((index % 160) * RECT_SIZE as usize) as i32,
                ((index / 160) * RECT_SIZE as usize) as i32,
                RECT_SIZE,
                RECT_SIZE,
            )
        })
        .collect::<Vec<Rect>>();

    // Load a font
    let mut font = ttf_context.load_font(font_path, 16)?;
    font.set_style(sdl2::ttf::FontStyle::NORMAL);

    let mut register_texture = get_font_texture(
        &texture_creator,
        &emulator.cpu.borrow().get_register_state(),
        &font,
    )?;
    let mut instruction_texture = get_font_texture(
        &texture_creator,
        &emulator.cpu.borrow().get_current_instruction_string(),
        &font,
    )?;

    let mut ppu_mode_texture =
        get_font_texture(&texture_creator, &emulator.ppu.mode.to_string(), &font)?;

    let TextureQuery {
        width: register_width,
        height: register_height,
        ..
    } = register_texture.query();
    let TextureQuery {
        width: instruction_width,
        height: instruction_height,
        ..
    } = instruction_texture.query();
    let TextureQuery {
        width: ppu_width,
        height: ppu_height,
        ..
    } = ppu_mode_texture.query();

    let register_target_rect = Rect::new(0, 300, register_width, register_height);
    let mut auto = true;

    let ppu_mode_rect = Rect::new(
        register_target_rect.x,
        register_target_rect.y + register_height as i32 + 12,
        ppu_width + 12,
        ppu_height + 12,
    );

    let instruction_target_rect = Rect::new(
        ((SCREEN_WIDTH - instruction_width) / 2) as i32,
        ppu_mode_rect.y + ppu_height as i32 + 20,
        instruction_width + 16,
        instruction_height,
    );

    'mainloop: loop {
        for event in sdl_context.event_pump()?.poll_iter() {
            match event {
                Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                }
                | Event::Quit { .. } => break 'mainloop,
                Event::KeyDown {
                    keycode: Some(Keycode::Right),
                    ..
                } => match emulator.step() {
                    Err(e) => {
                        eprintln!("{e}");
                        break 'mainloop;
                    }
                    _ => {}
                },
                Event::KeyDown {
                    keycode: Some(Keycode::P),
                    ..
                } => auto ^= true,
                _ => {}
            }
        }
        register_texture = get_font_texture(
            &texture_creator,
            &emulator.cpu.borrow().get_register_state(),
            &font,
        )?;
        instruction_texture = get_font_texture(
            &texture_creator,
            &emulator.cpu.borrow().get_current_instruction_string(),
            &font,
        )?;
        ppu_mode_texture =
            get_font_texture(&texture_creator, &emulator.ppu.mode.to_string(), &font)?;

        // if auto
        //     && emulator
        //         .cpu
        //         .borrow()
        //         .registers
        //         .get_16bit(crate::cpu_registers::Registers::PC)
        //         == 0x5D
        // {
        //     auto = false;
        // }
        if auto {
            match emulator.step() {
                Err(e) => {
                    eprintln!("{e}");
                    break 'mainloop;
                }
                _ => {}
            }
        }

        canvas.set_draw_color(Color::RGBA(195, 217, 255, 255));
        canvas.clear();

        // Draw the LCD
        if emulator.ppu.is_ppu_enabled() {
            emulator
                .ppu
                .get_lcd()
                .iter()
                .enumerate()
                .for_each(|(index, pixel)| {
                    canvas.set_draw_color(pixel.get_color());
                    canvas.fill_rect(lcd_rects[index]).unwrap();
                });
        }
        canvas
            .copy(&register_texture, None, Some(register_target_rect))
            .map_err(|e| e.to_string())?;
        canvas
            .copy(&instruction_texture, None, Some(instruction_target_rect))
            .map_err(|e| e.to_string())?;
        canvas
            .copy(&ppu_mode_texture, None, Some(ppu_mode_rect))
            .map_err(|e| e.to_string())?;
        canvas.present();
    }

    Ok(())
}
