use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::Color,
    rect::Rect,
    render::{Texture, TextureCreator, TextureQuery},
    surface::Surface,
    sys::SDL_Keycode,
    ttf::Font,
    video::WindowContext,
};

use crate::{cpu::Cpu, cpu_registers::CpuRegisters, memory::Memory, Wrapper};

const SCREEN_WIDTH: u32 = 600;
const SCREEN_HEIGHT: u32 = 600;

// Scale fonts to a reasonable size when they're too big (though they might look less smooth)
fn get_centered_rect(rect_width: u32, rect_height: u32, cons_width: u32, cons_height: u32) -> Rect {
    let wr = rect_width as f32 / cons_width as f32;
    let hr = rect_height as f32 / cons_height as f32;

    let (w, h) = if wr > 1f32 || hr > 1f32 {
        if wr > hr {
            println!("Scaling down! The text will look worse!");
            let h = (rect_height as f32 / wr) as i32;
            (cons_width as i32, h)
        } else {
            println!("Scaling down! The text will look worse!");
            let w = (rect_width as f32 / hr) as i32;
            (w, cons_height as i32)
        }
    } else {
        (rect_width as i32, rect_height as i32)
    };

    let cx = (SCREEN_WIDTH as i32 - w) / 2;
    let cy = (SCREEN_HEIGHT as i32 - h) / 2;
    Rect::new(cx, cy, w as u32, h as u32)
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

pub fn run(font_path: &str, cpu: Wrapper<Cpu<Memory>>) -> Result<(), String> {
    let sdl_context = sdl2::init()?;
    let video_subsys = sdl_context.video()?;
    let ttf_context = sdl2::ttf::init().map_err(|e| e.to_string())?;
    let mut cpu = cpu.borrow_mut();

    let window = video_subsys
        .window("SDL2_TTF Example", SCREEN_WIDTH, SCREEN_HEIGHT)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window.into_canvas().build().map_err(|e| e.to_string())?;
    let texture_creator = canvas.texture_creator();

    // Load a font
    let mut font = ttf_context.load_font(font_path, 16)?;
    font.set_style(sdl2::ttf::FontStyle::NORMAL);

    let mut register_texture =
        get_font_texture(&texture_creator, &cpu.get_register_state(), &font)?;
    let mut instruction_texture = get_font_texture(
        &texture_creator,
        &cpu.get_current_instruction_string(),
        &font,
    )?;

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
    let register_target_rect = Rect::new(0, 0, register_width, register_height);

    let instruction_target_rect = Rect::new(
        ((SCREEN_WIDTH - instruction_width) / 2) as i32,
        ((SCREEN_HEIGHT - instruction_height) / 2) as i32,
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
                } => match cpu.execute() {
                    Err(e) => {
                        eprintln!("{e}");
                        break 'mainloop;
                    }
                    _ => {}
                },
                _ => {}
            }
        }
        register_texture = get_font_texture(&texture_creator, &cpu.get_register_state(), &font)?;
        instruction_texture = get_font_texture(
            &texture_creator,
            &cpu.get_current_instruction_string(),
            &font,
        )?;
        if cpu.registers.get_16bit(crate::cpu_registers::Registers::PC) <= 0xA {
            match cpu.execute() {
                Err(e) => {
                    eprintln!("{e}");
                    break 'mainloop;
                }
                _ => {}
            }
        }

        canvas.set_draw_color(Color::RGBA(195, 217, 255, 255));
        canvas.clear();
        canvas
            .copy(&register_texture, None, Some(register_target_rect))
            .map_err(|e| e.to_string())?;
        canvas
            .copy(&instruction_texture, None, Some(instruction_target_rect))
            .map_err(|e| e.to_string())?;
        canvas.present();
    }

    Ok(())
}
