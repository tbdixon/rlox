use rlox::vm::VM;
use rlox::Result;
use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;

fn main() -> Result<()> {
    // Switch to set this via environment variables for quicker / cleaner toggling.
    // Requires unsafe code due to DEBUG being a global mutable variable, but no risk 
    // since this is only place it updates and reads are done through a function.
    if let Ok(debug) = env::var("RLOX_DEBUG") {
        unsafe{
            rlox::DEBUG = debug == "1";
        }
    }
    let args: Vec<String> = env::args().collect();
    // First arg is the binary name, if there are no other arguments run a REPL
    // If there is a single argument, parse that as the name of a Lox script
    // If there are more than one arguments, panic.
    match args.len() {
        1 => repl()?,
        2 => {
            let script =
                fs::read_to_string(&args[1]).expect(&format!("Unable to read script {}", args[1]));
            let mut vm = VM::new();
            vm.interpret(&script)?;
        }
        _ => {
            panic!("Invalid arguments received: {:?}", args);
        }
    };
    Ok(())
}

fn repl() -> Result<()> {
    let mut vm = VM::new();
    loop {
        let mut line = String::new();
        print!("> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        vm.interpret(&line[..])?;
        vm.reset_repl()?;
    }
}
