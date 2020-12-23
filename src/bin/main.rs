use rlox::vm::VM;
use rlox::Result;
use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;

fn main() -> Result<()> {
    /* Don't read from environment variable in --release. This ensures that the global rlox::DEBUG
     * is definitely false so compiler can get rid of all conditional checks (non-trivial cost with
     * every loop of the core VM run function). */
    #[cfg(debug_assertions)]
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
    }
}
