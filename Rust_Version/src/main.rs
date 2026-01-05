use optimal_mastermind::{Row, Space};
use std::time::Instant;

const N_COLORS: u8 = 5;
const N_PINS: u8 = 5;

fn main() {
    let mut space = Space::build(N_COLORS, N_PINS);
    let password = space.choose_password();
    let mut guess: Row;
    let start_time = Instant::now();
    loop {
        guess = space.run_turn(&password);
        println!("Guess was: {:?}", guess.get_vals());
        if space.get_space_size() <= 1 {
            break;
        }
        let duration = start_time.elapsed();
        println!("Time elapsed: {:?}", duration);
    }
    println!("Password was: {:?}", &password.get_vals());
}
