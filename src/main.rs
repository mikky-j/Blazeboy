use gameboy_emulator::{run, Emulator};
fn main() {
    // let filename = std::env::args().collect::<Vec<String>>();
    // let filename = &filename[1];
    // let mut emulator = match Emulator::new(&filename) {
    //     Ok(v) => v,
    //     Err(v) => {
    //         println!("{v}");
    //         return;
    //     }
    // };

    // match emulator.run() {
    //     Ok(()) => {}
    //     Err(v) => println!("{v}"),
    // }
    run("/home/king-mikky/Downloads/Lilex/Lilex Medium Nerd Font Complete.ttf").unwrap();
}
