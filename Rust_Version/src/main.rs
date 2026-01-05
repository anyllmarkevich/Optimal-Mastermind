use Rust_Version::{Row, Space};

fn main() {
    let mut space = Space::build(3, 3);
    let password = space.choose_password();
    let mut guess: Row;
    loop {
        guess = space.run_turn(&password);
        println!("Guess was: {:?}", guess.get_vals());
        if space.get_space_size() <= 1 {
            println!("SUCCESS!");
            break;
        }
    }
    println!("Guess was: {:?}", guess.get_vals());
    println!("Password was: {:?}", &password.get_vals());
}
