use gameboy_emulator::Emulator;
fn main() {
    let filename = std::env::args().collect::<Vec<String>>();
    let filename = &filename[1];
    let mut emulator = match Emulator::new(&filename) {
        Ok(v) => v,
        Err(v) => {
            println!("{v}");
            return;
        }
    };
    emulator.run();
}
