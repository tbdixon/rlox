use std::env;
use std::process;

use minigrep;
use minigrep::Config;

fn main() {
    let args: Vec<String> = env::args().collect();
    let cfg = Config::new(&args).unwrap_or_else(|err| {
        eprintln!("Error parsing arguments: {}\n", err);
        process::exit(1)
    });
    if let Err(err) = minigrep::run(cfg) {
        eprintln!("Error searching: {}", err)
    }
}
