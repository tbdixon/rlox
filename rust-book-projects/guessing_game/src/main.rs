use rand::Rng;
use std::cmp::Ordering;
use std::io;

fn main() {
    println!("Please enter a number:");

    let secret_number = rand::thread_rng().gen_range(1, 101);
    loop {
        let mut guess = String::new();  
        io::stdin()
            .read_line(&mut guess)
            .expect("Error reading line");
        
        let guess: u32 = match guess
            .trim()
            .parse() {
                Ok(num) => num,
                Err(_) => continue
            };

        match guess.cmp(&secret_number) {
            Ordering::Equal => {
                println!("Correct!");
                break;
            }
            Ordering::Less => println!("Too low"),
            Ordering::Greater => println!("Too high"),
        }
    }
}
